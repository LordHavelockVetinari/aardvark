use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;

#[derive(Clone)]
pub(crate) struct TestOutput(Rc<RefCell<Vec<u8>>>);

impl TestOutput {
    pub fn new() -> Self {
        Self(Rc::new(RefCell::new(vec![])))
    }

    pub fn assert_eq(&self, s: &str) {
        assert_eq!(std::str::from_utf8(&self.0.borrow()), Ok(s));
    }
}

impl Write for TestOutput {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.borrow_mut().write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

static SIXTERISK: &str = r#"
async asterisks() {
    print "*"
    await yield()
    await asterisks()
}

async pollv(task) {
    await poll(task, void())
}

async chain(task1, task2) {
    await task1
    return await task2
}

async polls(task, times) {
    await await poll(times, chain(pollv(task), polls(task, times)))
}

async three() {
    await yield()
    await yield()
    await yield()
}

async main() {
    await polls(asterisks(), chain(three(), three()))
}
"#;

#[test]
fn sixterisk() {
    let output = TestOutput::new();
    crate::run_str(SIXTERISK, Box::new(output.clone())).unwrap();
    output.assert_eq(&"*".repeat(6));
}
