#![allow(clippy::async_yields_async)]

use elsa::FrozenVec;
use futures::future::LocalBoxFuture;
use futures::{pin_mut, poll};
use std::cell::{Cell, RefCell, UnsafeCell};
use std::fmt::{self, Display};
use std::future::Future;
use std::io::Write;
use std::mem::ManuallyDrop;
use std::pin::Pin;
use std::rc::Rc;
use std::task::{Context, Poll};

pub enum Instruction<'a> {
    Move(u32),
    Copy(u32),
    Call(Cell<&'a Function<'a>>),
    Await,
    AwaitVoid,
    Output(&'a str),
}

impl<'a> Instruction<'a> {
    async fn run(&self, machine: &'a VirtualMachine<'a>, stack: &mut Vec<Option<Task<'a>>>) {
        use Instruction::*;
        match self {
            &Move(n) => {
                let value = stack[n as usize].take();
                while matches!(stack.last(), Some(None)) {
                    stack.pop();
                }
                debug_assert!(value.is_some());
                stack.push(value);
            }
            &Copy(n) => {
                let value = stack[n as usize].clone();
                debug_assert!(value.is_some());
                stack.push(value);
            }
            Call(func) => {
                let func = func.get();
                let first_arg_index = stack.len() - func.n_params as usize;
                let args = stack.drain(first_arg_index..).collect();
                while matches!(stack.last(), Some(None)) {
                    stack.pop();
                }
                stack.push(Some(func.call(machine, args)));
            }
            Await | AwaitVoid => {
                let task = stack
                    .pop()
                    .expect("nothing to await - stack is empty")
                    .expect("stack ends with None");
                while matches!(stack.last(), Some(None)) {
                    stack.pop();
                }
                let result = task.await;
                if matches!(self, Await) {
                    stack.push(Some(result));
                }
            }
            Output(s) => {
                let output = &mut *machine.output_handler.borrow_mut();
                write!(output, "{s}").unwrap();
                output.flush().unwrap();
            }
        }
    }
}

impl<'a> Display for Instruction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Instruction::*;
        match self {
            Copy(n) => write!(f, "COPY {n}"),
            Move(n) => write!(f, "MOVE {n}"),
            Call(func) => write!(f, "CALL {}", func.get().name),
            Await => write!(f, "AWAIT"),
            AwaitVoid => write!(f, "AWAITVOID"),
            Output(s) => write!(f, r#"OUTPUT {s:?}"#),
        }
    }
}

enum FunctionBehavior<'a> {
    Builtin(Box<dyn Fn(Vec<Option<Task<'a>>>) -> LocalBoxFuture<'a, Task<'a>> + 'a>),
    UserDefined(Vec<Instruction<'a>>),
}

pub struct Function<'a> {
    name: String,
    n_params: u32,
    returns_void: bool,
    behavior: FunctionBehavior<'a>,
}

impl<'a> Function<'a> {
    pub fn new_user_defined(name: String, n_params: u32, returns_void: bool) -> Self {
        Self {
            name,
            n_params,
            returns_void,
            behavior: FunctionBehavior::UserDefined(vec![]),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn n_params(&self) -> u32 {
        self.n_params
    }

    pub fn add_instruction(&mut self, instruction: Instruction<'a>) {
        match &mut self.behavior {
            FunctionBehavior::Builtin(_) => {
                panic!("Trying to add instructions to a builtin function.")
            }
            FunctionBehavior::UserDefined(instructions) => instructions.push(instruction),
        }
    }

    pub fn n_instructions(&self) -> Option<usize> {
        match &self.behavior {
            FunctionBehavior::Builtin(_) => None,
            FunctionBehavior::UserDefined(instructions) => Some(instructions.len()),
        }
    }

    pub fn instructions(&self) -> Option<&'a [Instruction]> {
        match &self.behavior {
            FunctionBehavior::Builtin(_) => None,
            FunctionBehavior::UserDefined(instructions) => Some(instructions),
        }
    }

    fn call(&'a self, vm: &'a VirtualMachine<'a>, args: Vec<Option<Task<'a>>>) -> Task<'a> {
        debug_assert_eq!(self.n_params as usize, args.len());
        debug_assert!(args.iter().all(|arg| arg.is_some()));
        Task {
            content: Rc::new(RefCell::new(match &self.behavior {
                FunctionBehavior::Builtin(f) => MultiPoll::Pending(Box::pin(f(args))),
                FunctionBehavior::UserDefined(instructions) => {
                    MultiPoll::Pending(Box::pin(async move {
                        let mut stack = args;
                        for ins in instructions {
                            ins.run(vm, &mut stack).await;
                        }
                        if self.returns_void {
                            Task::void()
                        } else {
                            stack
                                .pop()
                                .expect("stack is empty - cannot return")
                                .expect("top of stack is None")
                        }
                    }))
                }
            })),
        }
    }
}

impl<'a> Default for Function<'a> {
    fn default() -> Self {
        Self {
            name: String::new(),
            n_params: 0,
            returns_void: false,
            behavior: FunctionBehavior::UserDefined(vec![]),
        }
    }
}

pub struct VirtualMachine<'a> {
    strings: FrozenVec<String>,
    functions: ManuallyDrop<FrozenVec<Box<Function<'a>>>>,
    output_handler: RefCell<Box<dyn Write>>,
}

impl<'a> VirtualMachine<'a> {
    unsafe fn finalize(&mut self) {
        for func in self.functions.as_mut() {
            if let FunctionBehavior::UserDefined(code) = &mut func.behavior {
                code.clear()
            }
        }
        *self.functions.as_mut() = vec![];
    }

    pub fn with<F, T>(output_handler: Box<dyn Write>, f: F) -> T
    where
        F: FnOnce(&'a VirtualMachine<'a>) -> T,
    {
        let cell = UnsafeCell::new(Self {
            strings: FrozenVec::new(),
            functions: ManuallyDrop::new(FrozenVec::new()),
            output_handler: RefCell::new(output_handler),
        });
        // SAFETY: getting an immutable reference to a cell is generally safe,
        // although from now on the cell may reference itself,
        // so using it will be more dangerous.
        let result = unsafe { f(&*cell.get()) };
        // SAFETY: this technically breaks the reference aliasing rules.
        // However finalize() makes sure to delete each reference before
        // deleting the object it's pointing at, so this should be fine.
        unsafe {
            (*cell.get()).finalize();
        }
        result
    }

    pub fn is_empty(&self) -> bool {
        self.strings.is_empty() && self.functions.is_empty()
    }

    pub fn add_builtins(&'a self) {
        // WARNING: builtin functions cannot be recursive or reference the VM.
        // Otherwise they won't be destructed correctly.

        self.functions.push(Box::new(Function {
            name: "void".to_string(),
            n_params: 0,
            returns_void: true,
            behavior: FunctionBehavior::Builtin(Box::new(|_| Box::pin(async { Task::void() }))),
        }));

        self.functions.push(Box::new(Function {
            name: "yield".to_string(),
            n_params: 0,
            returns_void: true,
            behavior: FunctionBehavior::Builtin(Box::new(|_| {
                Box::pin(async {
                    futures::pending!();
                    Task::void()
                })
            })),
        }));

        self.functions.push(Box::new(Function {
            name: "ready".to_string(),
            n_params: 2,
            returns_void: false,
            behavior: FunctionBehavior::Builtin(Box::new(|v| {
                Box::pin(async move {
                    let task = v[0].as_ref().expect("first argument of ready() is None");
                    let default = v[1].as_ref().expect("second argument of ready() is None");
                    match &*task.content.borrow() {
                        MultiPoll::Ready(result) => result.clone(),
                        _ => default.clone(),
                    }
                })
            })),
        }));

        self.functions.push(Box::new(Function {
            name: "poll".to_string(),
            n_params: 2,
            returns_void: false,
            behavior: FunctionBehavior::Builtin(Box::new(|v| {
                Box::pin(async move {
                    let task = v[0]
                        .as_ref()
                        .expect("first argument of poll() is None")
                        .clone();
                    pin_mut!(task);
                    let default = v[1].as_ref().expect("second argument of poll() is None");
                    match poll!(task) {
                        Poll::Pending => default.clone(),
                        Poll::Ready(result) => result,
                    }
                })
            })),
        }));

        debug_assert_eq!(
            self.functions.len(),
            4,
            "Have you read the warning in the first line of this function?"
        );
    }

    pub fn get_function(&self, index: usize) -> &Function<'a> {
        &self.functions[index]
    }

    pub fn functions(&'a self) -> impl Iterator<Item = &'a Function> {
        self.functions.iter()
    }

    pub fn add_function(&self, func: Function<'a>) -> &Function<'a> {
        self.functions.push_get(Box::new(func))
    }

    pub fn add_string(&'a self, s: String) -> &'a str {
        self.strings.push_get(s)
    }

    pub fn run_main(&'a self, main: &'a Function<'a>) {
        debug_assert_eq!(main.n_params, 0);
        futures::executor::block_on(async {
            let main = main.call(self, vec![]);
            pin_mut!(main);
            loop {
                if poll!(main.as_mut()).is_ready() {
                    break;
                }
            }
        });
    }
}

// A future which may be polled multiple times, even after it finishes.
enum MultiPoll<'a, T> {
    Pending(LocalBoxFuture<'a, T>),
    Ready(T),
}

impl<'a, T: Clone + Unpin> Future for MultiPoll<'a, T> {
    type Output = T;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        match &mut *self {
            Self::Pending(fut) => match fut.as_mut().poll(cx) {
                Poll::Pending => Poll::Pending,
                Poll::Ready(x) => {
                    *self = Self::Ready(x.clone());
                    Poll::Ready(x)
                }
            },
            Self::Ready(x) => Poll::Ready(x.clone()),
        }
    }
}

#[derive(Clone)]
struct Task<'a> {
    content: Rc<RefCell<MultiPoll<'a, Task<'a>>>>,
}

impl<'a> Task<'a> {
    fn void() -> Self {
        Task {
            content: Rc::new(RefCell::new(MultiPoll::Pending(Box::pin(async {
                Task::void()
            })))),
        }
    }
}

impl<'a> Future for Task<'a> {
    type Output = Task<'a>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        Pin::new(&mut *self.content.borrow_mut()).poll(cx)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tests::TestOutput;

    #[test]
    fn basic_test() {
        use Instruction::*;
        let output = TestOutput::new();
        VirtualMachine::with(Box::new(output.clone()), |vm| {
            vm.add_builtins();
            vm.strings.push("Hello, World!\n".to_string());
            vm.strings.push("*".to_string());
            let func = |index| Cell::new(&vm.functions[index]);
            // 4 - Hello, World!
            vm.functions.push(Box::new(Function {
                name: "hello".to_string(),
                n_params: 0,
                returns_void: true,
                behavior: FunctionBehavior::UserDefined(vec![Output(&vm.strings[0])]),
            }));
            // 5 - three polls
            vm.functions.push(Box::new(Function {
                name: "thrice".to_string(),
                n_params: 1,
                returns_void: true,
                behavior: FunctionBehavior::UserDefined(vec![
                    Copy(0),
                    Call(func(0)),
                    Call(func(3)),
                    AwaitVoid,
                    Copy(0),
                    Call(func(0)),
                    Call(func(3)),
                    AwaitVoid,
                    Call(func(0)),
                    Call(func(3)),
                    AwaitVoid,
                ]),
            }));
            // 6 - infinite asterisks
            vm.functions.push(Box::new(Function {
                name: "asterisks".to_string(),
                n_params: 0,
                returns_void: true,
                behavior: FunctionBehavior::UserDefined(vec![
                    Output(&vm.strings[1]),
                    Call(func(1)),
                    AwaitVoid,
                    Call(func(0)),
                    AwaitVoid,
                ]),
            }));
            // 7 - three asterisk
            match &vm.functions.last().unwrap().behavior {
                FunctionBehavior::UserDefined(v) => match &v[3] {
                    Call(cell) => cell.set(&vm.functions[6]),
                    _ => unreachable!(),
                },
                FunctionBehavior::Builtin(_) => unreachable!(),
            }
            vm.functions.push(Box::new(Function {
                name: "threeAsterisks".to_string(),
                n_params: 0,
                returns_void: true,
                behavior: FunctionBehavior::UserDefined(vec![
                    Call(func(6)),
                    Call(func(5)),
                    AwaitVoid,
                ]),
            }));
            futures::executor::block_on(vm.functions[4].call(&vm, vec![]));
            futures::executor::block_on(vm.functions[7].call(&vm, vec![]));
            output.assert_eq("Hello, World!\n***");
        });
    }
}
