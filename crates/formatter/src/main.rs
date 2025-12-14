use amber_fmt::Output;
use amber_grammar::{Grammar, LSPAnalysis as _, alpha040::AmberCompiler};

fn main() {
    let data = include_str!("../../../run_coverage.ab");

    let amber_compiler = AmberCompiler::new();
    let tokenize = amber_compiler.tokenize(data);
    let parse = amber_compiler.parse(&tokenize);

    println!("{:?}", parse.ast);
    match parse.ast {
        Grammar::Alpha034(_items) => todo!(),
        Grammar::Alpha035(_items) => todo!(),
        Grammar::Alpha040(items) => {
            if let Some(items) = items {
                {
                    let mut output = Output::default();
                    for item in items {
                        use amber_fmt::SpanTextOutput;
                        (&item).output(&mut output);
                    }
                    let format = output.format(data).expect("Able to parse");
                    println!("{format}");
                }
            }
        }
        Grammar::Alpha050(_items) => todo!(),
    }
}
