use inkwell::context::Context;
use inkwell::passes::PassManager;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
};
use inkwell::OptimizationLevel;

use crate::ast::{ExprKind, Item, ItemKind, StmtKind};

pub struct NativeBackend {
    context: Context,
}

impl NativeBackend {
    pub fn new() -> Self {
        Self {
            context: Context::create(),
        }
    }

    pub fn compile(&self, program: Vec<Item>) {
        Target::initialize_aarch64(&InitializationConfig::default());

        let opt = OptimizationLevel::Default;
        let reloc = RelocMode::Default;
        let model = CodeModel::Default;

        let target = Target::from_name("aarch64").expect("Failed to parse target");

        let target = dbg!(target);

        let target_machine = target
            .create_target_machine(
                &TargetTriple::create("aarch64-apple-darwin"),
                "apple-m2",
                "",
                opt,
                reloc,
                model,
            )
            .unwrap();

        let target_machine = dbg!(target_machine);

        let module = self.context.create_module("main");
        let builder = self.context.create_builder();

        let fpm = PassManager::create(&module);

        fpm.add_instruction_combining_pass();

        fpm.initialize();

        for item in program
            // HACK: Reverse the items so we define the helper functions before `main`.
            // This should be replaced with a call-flow graph.
            .into_iter()
            .rev()
        {
            match item.kind {
                ItemKind::Fn(fun) => {
                    let fn_type = self.context.void_type().fn_type(&[], false);

                    let fn_value = module.add_function(&item.name.to_string(), fn_type, None);

                    let entry = self.context.append_basic_block(fn_value, "entry");

                    builder.position_at_end(entry);

                    for stmt in fun.body {
                        match stmt.kind {
                            StmtKind::Expr(expr) => match expr.kind {
                                ExprKind::Literal(_) => todo!(),
                                ExprKind::Variable { name } => todo!(),
                                ExprKind::Call { fun, args } => {
                                    let callee_name = match fun.kind {
                                        ExprKind::Variable { name } => name,
                                        _ => todo!(),
                                    };

                                    if let Some(callee) =
                                        module.get_function(&callee_name.to_string())
                                    {
                                        builder.build_call(callee, &[], "tmp");
                                    } else {
                                        eprintln!("Function '{}' not found.", callee_name);
                                    }
                                }
                            },
                            StmtKind::Item(item) => todo!(),
                        }
                    }

                    builder.build_return(None);

                    dbg!(fn_value);

                    if fn_value.verify(true) {
                        fpm.run_on(&fn_value);

                        println!("{} is verified!", item.name);
                    } else {
                        println!("{} is not verified :(", item.name);
                    }

                    fn_value.print_to_stderr();
                }
            }
        }

        dbg!(module.get_functions().count());

        let buffer = target_machine
            .write_to_memory_buffer(&module, FileType::Object)
            .expect("Failed to write to buffer");

        use std::io::Write;

        let mut outfile = std::fs::File::create("build/main.o").unwrap();

        outfile.write_all(buffer.as_slice()).unwrap();

        let bitcode = module.write_bitcode_to_memory();

        outfile.write_all(bitcode.as_slice()).unwrap();
    }
}
