use amber_grammar::{Grammar, LSPAnalysis as _, alpha040::AmberCompiler};

fn main() {
    let Some(file_path) = std::env::args().skip(1).next() else {
        eprintln!("No files to format");
        return;
    };

    let Ok(data) = std::fs::read_to_string(&file_path)
        .inspect_err(|err| eprintln!("Unable to read file '{file_path}' err: {err}"))
    else {
        return;
    };

    let amber_compiler = AmberCompiler::new();
    let tokenize = amber_compiler.tokenize(&data);
    let parse = amber_compiler.parse(&tokenize);

    match parse.ast {
        Grammar::Alpha034(_items) => todo!(),
        Grammar::Alpha035(_items) => todo!(),
        Grammar::Alpha040(items) => {
            if let Some(items) = items {
                {
                    let format = amber_fmt::format(&items, &data).expect("Able to parse");
                    println!("{format}");
                }
            }
        }
        Grammar::Alpha050(_items) => todo!(),
    }
}
