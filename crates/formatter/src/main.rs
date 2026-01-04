use amber_grammar::{Grammar, LSPAnalysis as _, alpha040::AmberCompiler};

fn main() {
    // let data = include_str!("../../../run_coverage.ab");
    let data = include_str!("../../../resources/alpha040/std/fs.ab");

    let amber_compiler = AmberCompiler::new();
    let tokenize = amber_compiler.tokenize(data);
    let parse = amber_compiler.parse(&tokenize);

    match parse.ast {
        Grammar::Alpha034(_items) => todo!(),
        Grammar::Alpha035(_items) => todo!(),
        Grammar::Alpha040(items) => {
            if let Some(items) = items {
                {
                    let format = amber_fmt::format(&items, data).expect("Able to parse");
                    println!("{format}");
                }
            }
        }
        Grammar::Alpha050(_items) => todo!(),
    }
}
