use crate::parse::{self, Expression, Position, StartEnd};
use crate::vm::{Function, Instruction, VirtualMachine};
use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::{self, Display};

#[derive(Debug)]
enum CompilationErrorContent {
    NoMainFunction,
    MainHasParams,
    VarDoesntExist(String),
    FuncDoesntExist(String),
    DuplicateFunc(String),
    DuplicateParam(String, String),
    WrongNArguments(String, usize, usize),
}

impl Display for CompilationErrorContent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use CompilationErrorContent::*;
        match self {
            NoMainFunction => write!(f, "No main function."),
            MainHasParams => write!(f, "main() can't take any parameters."),
            VarDoesntExist(var) => write!(f, "Variable '{var}' doesn't exist."),
            FuncDoesntExist(func) => write!(f, "Function '{func}' doesn't exist."),
            DuplicateFunc(func) => write!(f, "Can't define function '{func}' more than once."),
            DuplicateParam(func, param) => {
                write!(f, "Duplicate parameter '{param}' in function '{func}'.")
            }
            WrongNArguments(func, args, correct) => write!(
                f,
                "Function '{func}' called with {args} argument(s) (instead of {correct})."
            ),
        }
    }
}

#[derive(Debug)]
pub struct CompilationError {
    location: StartEnd,
    content: CompilationErrorContent,
}

impl Display for CompilationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Compilation Error at {}: {}",
            self.location, self.content
        )
    }
}

impl Error for CompilationError {}

struct IncorrectCallInstruction {
    instruction_index: usize,
    required_function_index: usize,
}

struct IncorrectFunctionReference<'a> {
    incorrect_reference: &'a Cell<&'a Function<'a>>,
    required_function_index: usize,
}

impl<'a> IncorrectFunctionReference<'a> {
    fn correct(&self, vm: &'a VirtualMachine<'a>) {
        self.incorrect_reference
            .set(vm.get_function(self.required_function_index));
    }
}

struct FunctionInfo {
    index: usize,
    n_params: usize,
}

struct VaraibleInfo {
    stack_pos: usize,
    last_use: Option<Position>,
}

struct StackValue;

pub type MainFunction<'a> = Function<'a>;

type Result<T> = std::result::Result<T, CompilationError>;

struct FunctionCompiler<'a, 'b> {
    func: Function<'a>,
    vm: &'a VirtualMachine<'a>,
    func_table: &'b HashMap<String, FunctionInfo>,
    current_vars: HashMap<String, usize>,
    var_list: Vec<VaraibleInfo>,
    n_used_vars: usize,
    current_stack: Vec<Option<StackValue>>,
    dummy_function: &'a Function<'a>,
    incorrect_calls: Vec<IncorrectCallInstruction>,
}

impl<'a, 'b> FunctionCompiler<'a, 'b> {
    fn new(vm: &'a VirtualMachine<'a>, func_table: &'b HashMap<String, FunctionInfo>) -> Self {
        Self {
            func: Function::default(),
            vm,
            func_table,
            current_vars: HashMap::new(),
            var_list: vec![],
            n_used_vars: 0,
            current_stack: vec![],
            dummy_function: vm.get_function(0),
            incorrect_calls: vec![],
        }
    }

    fn find_last_uses_expr(&mut self, expr: &parse::Expression) -> Result<()> {
        use parse::ExpressionContent::*;
        match &expr.content {
            Await(val) => {
                self.find_last_uses_expr(val)?;
            }
            Call(_, args) => {
                for arg in args {
                    self.find_last_uses_expr(arg)?;
                }
            }
            Varaible(v) => {
                let var_index =
                    *self
                        .current_vars
                        .get(&v.name)
                        .ok_or_else(|| CompilationError {
                            location: expr.location.clone(),
                            content: CompilationErrorContent::VarDoesntExist(v.name.clone()),
                        })?;
                self.var_list[var_index].last_use = Some(expr.location.start.clone());
            }
        }
        Ok(())
    }

    // Finds the last expression which uses each varaible.
    // Returns Err if some varaible doesn't exist.
    fn find_last_uses(&mut self, parsed_func: &parse::Function) -> Result<()> {
        use parse::StatementContent::*;
        for param in &parsed_func.params {
            self.current_vars
                .insert(param.name.clone(), self.var_list.len());
            self.var_list.push(VaraibleInfo {
                stack_pos: 0,
                last_use: None,
            });
        }
        for stmt in &parsed_func.code {
            match &stmt.content {
                Assignment(var, val) => {
                    self.find_last_uses_expr(val)?;
                    self.current_vars
                        .insert(var.name.clone(), self.var_list.len());
                    self.var_list.push(VaraibleInfo {
                        stack_pos: 0,
                        last_use: None,
                    });
                }
                Expression(val) | Return(val) => {
                    self.find_last_uses_expr(val)?;
                }
                Print(_) | PrintLn(_) => {}
            }
        }
        self.current_vars.clear();
        Ok(())
    }

    fn expect_var_exists(&self, var: &parse::Varaible, location: &StartEnd) -> Result<()> {
        self.current_vars
            .contains_key(&var.name)
            .then_some(())
            .ok_or_else(|| CompilationError {
                location: location.clone(),
                content: CompilationErrorContent::VarDoesntExist(var.name.clone()),
            })
    }

    fn expect_func_exists(&self, func: &parse::Varaible, location: &StartEnd) -> Result<()> {
        self.func_table
            .contains_key(&func.name)
            .then_some(())
            .ok_or_else(|| CompilationError {
                location: location.clone(),
                content: CompilationErrorContent::FuncDoesntExist(func.name.clone()),
            })
    }

    fn expect_no_dup_params(&self, func: &parse::Function) -> Result<()> {
        let mut params = HashSet::new();
        for param in &func.params {
            if !params.insert(&param.name) {
                return Err(CompilationError {
                    location: func.location.clone(),
                    content: CompilationErrorContent::DuplicateParam(
                        func.name.clone(),
                        param.name.clone(),
                    ),
                });
            }
        }
        Ok(())
    }

    fn add_move_instruction(&mut self, from: usize) {
        if from == self.current_stack.len() - 1 {
            return;
        }
        self.func.add_instruction(Instruction::Move(from as u32));
        self.current_stack[from] = None;
        self.current_stack.push(Some(StackValue));
    }

    fn check_n_args(
        &self,
        func: &parse::Varaible,
        params: &[Expression],
        location: &StartEnd,
    ) -> Result<()> {
        let correct_number = self.func_table[&func.name].n_params;
        if params.len() != correct_number {
            return Err(CompilationError {
                location: location.clone(),
                content: CompilationErrorContent::WrongNArguments(
                    func.name.clone(),
                    params.len(),
                    correct_number,
                ),
            });
        }
        Ok(())
    }

    fn add_expr(&mut self, expr: parse::Expression) -> Result<()> {
        use parse::ExpressionContent::*;
        match expr.content {
            Await(expr2) => {
                self.add_expr(*expr2)?;
                self.func.add_instruction(Instruction::Await);
            }
            Call(func, args) => {
                self.expect_func_exists(&func, &expr.location)?;
                self.check_n_args(&func, &args, &expr.location)?;
                for param in args {
                    self.add_expr(param)?;
                }
                self.incorrect_calls.push(IncorrectCallInstruction {
                    instruction_index: self.func.n_instructions().unwrap(),
                    required_function_index: self.func_table[&func.name].index,
                });
                self.func
                    .add_instruction(Instruction::Call(Cell::new(self.dummy_function)));
                let n_params = self.func_table[&func.name].n_params;
                for _ in 0..n_params {
                    let popped = self.current_stack.pop();
                    debug_assert!(matches!(popped, Some(Some(StackValue))));
                }
                while matches!(self.current_stack.last(), Some(None)) {
                    self.current_stack.pop();
                }
                self.current_stack.push(Some(StackValue));
            }
            Varaible(var) => {
                self.expect_var_exists(&var, &expr.location)?;
                let var_info = &self.var_list[self.current_vars[&var.name]];
                if var_info.last_use == Some(expr.location.start) {
                    self.add_move_instruction(var_info.stack_pos);
                } else {
                    self.func
                        .add_instruction(Instruction::Copy(var_info.stack_pos as u32));
                    self.current_stack.push(Some(StackValue));
                }
            }
        }
        Ok(())
    }

    fn warn_unused(&self, location: &StartEnd) {
        eprintln!("Warning: unused expression at {location}. (did you forget 'await'?)");
    }

    fn add_expr_void(&mut self, expr: parse::Expression, warn_unused: bool) -> Result<()> {
        use parse::ExpressionContent::*;
        match expr.content {
            Await(expr2) => {
                self.add_expr(*expr2)?;
                self.func.add_instruction(Instruction::AwaitVoid);
                loop {
                    self.current_stack.pop();
                    if !matches!(self.current_stack.last(), Some(None)) {
                        break;
                    }
                }
            }
            Call(func, args) => {
                self.expect_func_exists(&func, &expr.location)?;
                self.check_n_args(&func, &args, &expr.location)?;
                if warn_unused {
                    self.warn_unused(&expr.location);
                }
                for arg in args {
                    self.add_expr_void(arg, false)?;
                }
            }
            Varaible(var) => {
                self.expect_var_exists(&var, &expr.location)?;
                if warn_unused {
                    self.warn_unused(&expr.location);
                }
            }
        }
        Ok(())
    }

    fn add_statement(&mut self, stmt: parse::Statement) -> Result<()> {
        use parse::StatementContent::*;
        match stmt.content {
            Expression(expr) => self.add_expr_void(expr, true)?,
            Assignment(var, val) => {
                if self.var_list[self.n_used_vars].last_use.is_some() {
                    self.add_expr(val)?;
                    self.current_vars.insert(var.name, self.n_used_vars);
                    self.var_list[self.n_used_vars].stack_pos = self.current_stack.len() - 1;
                    self.n_used_vars += 1;
                } else {
                    eprintln!(
                        "Warning: unused variable '{}' (defined at {}).",
                        var.name, stmt.location
                    );
                    self.add_expr_void(val, false)?;
                    self.n_used_vars += 1;
                }
            }
            Print(s) => {
                let s = self.vm.add_string(s.content);
                self.func.add_instruction(Instruction::Output(s));
            }
            PrintLn(s) => {
                let s = self.vm.add_string(s.content + "\n");
                self.func.add_instruction(Instruction::Output(s));
            }
            Return(expr) => self.add_expr(expr)?,
        }
        Ok(())
    }

    fn compile(&mut self, mut parsed_func: parse::Function) -> Result<()> {
        self.expect_no_dup_params(&parsed_func)?;
        let return_pos = parsed_func
            .code
            .iter()
            .position(|stmt| matches!(stmt.content, parse::StatementContent::Return(_)));
        match return_pos {
            Some(pos) if pos < parsed_func.code.len() - 1 => {
                eprintln!(
                    "Warning: unreachable code detected in {}.",
                    parsed_func.name
                );
                parsed_func.code.truncate(pos + 1);
            }
            _ => {}
        }
        self.find_last_uses(&parsed_func)?;
        let n_params = parsed_func.params.len();
        self.func = Function::new_user_defined(
            parsed_func.name.clone(),
            n_params as u32,
            return_pos.is_none(),
        );
        for (i, param) in parsed_func.params.into_iter().enumerate() {
            self.current_stack.push(Some(StackValue));
            self.current_vars.insert(param.name, i);
            self.var_list[i].stack_pos = i;
        }
        self.n_used_vars = n_params;
        for stmt in parsed_func.code {
            self.add_statement(stmt)?;
        }
        debug_assert_eq!(self.n_used_vars, self.var_list.len());
        Ok(())
    }
}

struct Compiler<'a> {
    vm: &'a VirtualMachine<'a>,
    func_table: HashMap<String, FunctionInfo>,
    incorrect_refs: Vec<IncorrectFunctionReference<'a>>,
}

impl<'a> Compiler<'a> {
    fn compile_function(&mut self, parsed_func: parse::Function) -> Result<()> {
        let mut comp = FunctionCompiler::new(self.vm, &self.func_table);
        comp.compile(parsed_func)?;
        let func = self.vm.add_function(comp.func);
        let instructions = func.instructions();
        for call in comp.incorrect_calls {
            let incorrect_reference = match &instructions.unwrap()[call.instruction_index] {
                Instruction::Call(r) => r,
                _ => panic!("Expected a Call instruction."),
            };
            self.incorrect_refs.push(IncorrectFunctionReference {
                incorrect_reference,
                required_function_index: call.required_function_index,
            });
        }
        Ok(())
    }

    fn compile(&mut self, prog: parse::Program) -> Result<&'a MainFunction<'a>> {
        assert!(self.vm.is_empty());
        assert!(self.func_table.is_empty());
        assert!(self.incorrect_refs.is_empty());
        self.vm.add_builtins();
        for (i, func) in self.vm.functions().enumerate() {
            self.func_table.insert(
                func.name().to_string(),
                FunctionInfo {
                    index: i,
                    n_params: func.n_params() as usize,
                },
            );
        }
        for (func, i) in prog.functions.iter().zip(self.func_table.len()..) {
            if self.func_table.contains_key(&func.name) {
                return Err(CompilationError {
                    location: func.location.clone(),
                    content: CompilationErrorContent::DuplicateFunc(func.name.clone()),
                });
            }
            self.func_table.insert(
                func.name.clone(),
                FunctionInfo {
                    index: i,
                    n_params: func.params.len(),
                },
            );
        }
        match self.func_table.get("main") {
            None => {
                return Err(CompilationError {
                    location: StartEnd::default(),
                    content: CompilationErrorContent::NoMainFunction,
                })
            }
            Some(f) if f.n_params > 0 => {
                return Err(CompilationError {
                    location: prog
                        .functions
                        .iter()
                        .find(|f| f.name == "main")
                        .unwrap()
                        .location
                        .clone(),
                    content: CompilationErrorContent::MainHasParams,
                })
            }
            Some(_) => {}
        }
        for func in prog.functions {
            self.compile_function(func)?;
        }
        for r in &mut self.incorrect_refs {
            r.correct(self.vm);
        }
        #[cfg(feature = "print_vm_assembly")]
        for func in self.vm.functions() {
            println!("{}:", func.name());
            if let Some(instructions) = func.instructions() {
                for ins in instructions {
                    println!("{ins}");
                }
                println!("RET");
            } else {
                println!("BUILTIN");
            }
            println!();
        }
        Ok(self.vm.get_function(self.func_table["main"].index))
    }
}

pub fn compile<'a>(
    prog: parse::Program,
    vm: &'a VirtualMachine<'a>,
) -> Result<&'a MainFunction<'a>> {
    Compiler {
        vm,
        func_table: HashMap::new(),
        incorrect_refs: vec![],
    }
    .compile(prog)
}
