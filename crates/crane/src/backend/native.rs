use std::process::Command;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::passes::PassManager;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
};
use inkwell::types::BasicType;
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, CallSiteValue, FunctionValue, GlobalValue, IntValue,
};
use inkwell::{AddressSpace, OptimizationLevel};
use thin_vec::ThinVec;

use crate::ast::{Expr, ExprKind, FnParam, Item, ItemKind, Literal, LiteralKind, StmtKind};

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

        // Define `puts`.
        {
            let i8_type = self.context.i8_type();
            let i32_type = self.context.i32_type();
            let fn_type = i32_type.fn_type(
                &[i8_type
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum()
                    .into()],
                false,
            );

            let puts = module.add_function("puts", fn_type, Some(Linkage::External));

            dbg!(puts);
        }

        // Define `sprintf`.
        {
            let i8_type = self.context.i8_type();
            let i32_type = self.context.i32_type();

            let fn_type = i32_type.fn_type(
                &[
                    i8_type
                        .ptr_type(AddressSpace::default())
                        .as_basic_type_enum()
                        .into(),
                    i8_type
                        .ptr_type(AddressSpace::default())
                        .as_basic_type_enum()
                        .into(),
                ],
                true,
            );

            let sprintf = module.add_function("sprintf", fn_type, Some(Linkage::External));

            dbg!(sprintf);
        }

        // Define `printf`.
        {
            let i8_type = self.context.i8_type();
            let i32_type = self.context.i32_type();

            let fn_type = i32_type.fn_type(
                &[i8_type
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum()
                    .into()],
                true,
            );

            let printf = module.add_function("printf", fn_type, Some(Linkage::External));

            dbg!(printf);
        }

        // Define `print`.
        {
            let fn_name = "print";

            let i8_type = self.context.i8_type();

            let fn_type = self.context.void_type().fn_type(
                &[i8_type
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum()
                    .into()],
                false,
            );

            let fn_value = module.add_function(&fn_name, fn_type, None);

            let value_param = fn_value.get_first_param().unwrap();

            let entry = self.context.append_basic_block(fn_value, "entry");

            builder.position_at_end(entry);

            let template = b"%1$s";

            let i8_type = self.context.i8_type();
            let i8_array_type = i8_type.array_type(template.len() as u32 + 1);

            let template = self.context.const_string(template, true);

            let global = module.add_global(i8_array_type, None, "print_template");
            global.set_linkage(Linkage::Internal);
            global.set_constant(true);
            global.set_initializer(&template);

            if let Some(callee) = module.get_function(&"printf") {
                builder.build_call(
                    callee,
                    &[global.as_basic_value_enum().into(), value_param.into()],
                    "tmp",
                );
            } else {
                eprintln!("Function '{}' not found.", "puts");
            }

            builder.build_return(None);

            if fn_value.verify(true) {
                fpm.run_on(&fn_value);

                println!("{} is verified!", fn_name);
            } else {
                println!("{} is not verified :(", fn_name);
            }

            dbg!(fn_value);
        }

        // Define `println`.
        {
            let fn_name = "println";

            let i8_type = self.context.i8_type();

            let fn_type = self.context.void_type().fn_type(
                &[i8_type
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum()
                    .into()],
                false,
            );

            let fn_value = module.add_function(&fn_name, fn_type, None);

            let value_param = fn_value.get_first_param().unwrap();

            let entry = self.context.append_basic_block(fn_value, "entry");

            builder.position_at_end(entry);

            if let Some(callee) = module.get_function(&"puts") {
                builder.build_call(callee, &[value_param.into()], "tmp");
            } else {
                eprintln!("Function '{}' not found.", "puts");
            }

            builder.build_return(None);

            if fn_value.verify(true) {
                fpm.run_on(&fn_value);

                println!("{} is verified!", fn_name);
            } else {
                println!("{} is not verified :(", fn_name);
            }

            dbg!(fn_value);
        }

        // Define `int_add`.
        {
            let fn_name = "int_add";

            let i128_type = self.context.i128_type();

            let fn_type = self.context.i128_type().fn_type(
                &[
                    i128_type.as_basic_type_enum().into(),
                    i128_type.as_basic_type_enum().into(),
                ],
                false,
            );

            let fn_value = module.add_function(&fn_name, fn_type, None);

            let lhs_param = fn_value.get_first_param().unwrap().into_int_value();
            let rhs_param = fn_value.get_nth_param(1).unwrap().into_int_value();

            let entry = self.context.append_basic_block(fn_value, "entry");

            builder.position_at_end(entry);

            let sum = builder.build_int_add(lhs_param, rhs_param, "sum");

            builder.build_return(Some(&sum));

            if fn_value.verify(true) {
                fpm.run_on(&fn_value);

                println!("{} is verified!", fn_name);
            } else {
                println!("{} is not verified :(", fn_name);
            }

            dbg!(fn_value);
        }

        // Define `int_to_string`.
        {
            let fn_name = "int_to_string";

            let i128_type = self.context.i128_type();
            let i8_type = self.context.i8_type();
            let i8_ptr_type = i8_type.ptr_type(AddressSpace::default());

            let fn_type = i8_ptr_type.fn_type(&[i128_type.as_basic_type_enum().into()], false);

            let fn_value = module.add_function(&fn_name, fn_type, None);

            let int_value = fn_value.get_first_param().unwrap().into_int_value();

            let entry = self.context.append_basic_block(fn_value, "entry");

            builder.position_at_end(entry);

            let buffer = builder
                .build_malloc(i8_ptr_type, "buffer")
                .expect("Failed to allocate `int_to_string` buffer.");

            let template = b"%1$d";

            let i8_type = self.context.i8_type();
            let i8_array_type = i8_type.array_type(template.len() as u32 + 1);

            let template = self.context.const_string(template, true);

            let global = module.add_global(i8_array_type, None, "int_to_string_template");
            global.set_linkage(Linkage::Internal);
            global.set_constant(true);
            global.set_initializer(&template);

            if let Some(callee) = module.get_function(&"sprintf") {
                builder.build_call(
                    callee,
                    &[
                        buffer.into(),
                        global.as_basic_value_enum().into(),
                        int_value.into(),
                    ],
                    "tmp",
                );
            } else {
                panic!("Function '{}' not found.", "sprintf");
            }

            builder.build_return(Some(&buffer));

            if fn_value.verify(true) {
                fpm.run_on(&fn_value);

                println!("{} is verified!", fn_name);
            } else {
                println!("{} is not verified :(", fn_name);
            }

            dbg!(fn_value);
        }

        for item in program
            // HACK: Reverse the items so we define the helper functions before `main`.
            // This should be replaced with a call graph.
            .into_iter()
            .rev()
        {
            match item.kind {
                ItemKind::Fn(fun) => {
                    let params = fun
                        .params
                        .iter()
                        .map(|_param| {
                            let i8_type = self.context.i8_type();

                            i8_type
                                .ptr_type(AddressSpace::default())
                                .as_basic_type_enum()
                                .into()
                        })
                        .collect::<Vec<_>>();

                    let fn_type = self.context.void_type().fn_type(&params, false);

                    let fn_value = module.add_function(&item.name.to_string(), fn_type, None);

                    for (index, param_value) in fn_value.get_param_iter().enumerate() {
                        if let Some(param) = fun.params.get(index) {
                            param_value.set_name(&param.name.to_string());
                        }
                    }

                    let entry = self.context.append_basic_block(fn_value, "entry");

                    builder.position_at_end(entry);

                    for stmt in fun.body {
                        match stmt.kind {
                            StmtKind::Expr(expr) => Self::compile_expr(
                                &self.context,
                                &builder,
                                &module,
                                &fun.params,
                                &fn_value,
                                expr,
                            ),
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

        module
            .print_to_file("build/main.ll")
            .expect("Failed to emit main.ll");

        let buffer = target_machine
            .write_to_memory_buffer(&module, FileType::Object)
            .expect("Failed to write to buffer");

        use std::io::Write;

        let mut outfile = std::fs::File::create("build/main.o").unwrap();

        outfile.write_all(buffer.as_slice()).unwrap();

        let bitcode = module.write_bitcode_to_memory();

        outfile.write_all(bitcode.as_slice()).unwrap();

        let exit_status = Command::new("clang")
            .args(["-o", "build/main", "build/main.o"])
            .status()
            .expect("Failed to build with clang");

        println!("clang exited with {}", exit_status);
    }

    fn compile_expr<'ctx>(
        context: &'ctx Context,
        builder: &Builder<'ctx>,
        module: &Module<'ctx>,
        fn_params: &ThinVec<FnParam>,
        fn_value: &FunctionValue<'ctx>,
        expr: Expr,
    ) {
        match expr.kind {
            ExprKind::Literal(literal) => match literal.kind {
                LiteralKind::String => {
                    Self::compile_string_literal(&context, &builder, &module, literal);
                }
                LiteralKind::Integer => {}
            },
            ExprKind::Variable { name } => todo!(),
            ExprKind::Call { fun, args } => {
                Self::compile_fn_call(
                    context,
                    builder,
                    module,
                    fn_value,
                    fn_params,
                    fun.clone(),
                    args,
                )
                .expect(&format!("Failed to compile function call: {:?}", fun));
            }
        }
    }

    fn compile_string_literal<'ctx>(
        context: &'ctx Context,
        builder: &Builder<'ctx>,
        module: &Module<'ctx>,
        literal: Literal,
    ) -> GlobalValue<'ctx> {
        // Unquote the string literal.
        let value = {
            let mut chars = literal.value.chars();
            chars.next();
            chars.next_back();
            chars.as_str()
        };

        let value = value.as_bytes();

        let i8_type = context.i8_type();
        let i8_array_type = i8_type.array_type(value.len() as u32 + 1);

        let string = context.const_string(value, true);

        let global = module.add_global(i8_array_type, None, "string_lit");
        global.set_linkage(Linkage::Internal);
        global.set_constant(true);
        global.set_initializer(&string);

        global
    }

    fn compile_integer_literal<'ctx>(
        context: &'ctx Context,
        builder: &Builder<'ctx>,
        module: &Module<'ctx>,
        literal: Literal,
    ) -> IntValue<'ctx> {
        let int_type = match literal.kind {
            LiteralKind::Integer => context.i128_type(),
            LiteralKind::String => unreachable!("Tried to compile a string literal as an integer."),
        };

        int_type.const_int(literal.value.parse().unwrap(), false)
    }

    fn compile_fn_call<'ctx>(
        context: &'ctx Context,
        builder: &Builder<'ctx>,
        module: &Module<'ctx>,
        caller: &FunctionValue<'ctx>,
        caller_params: &ThinVec<FnParam>,
        fun: Box<Expr>,
        args: ThinVec<Box<Expr>>,
    ) -> Result<CallSiteValue<'ctx>, String> {
        let callee_name = match fun.kind {
            ExprKind::Variable { name } => name,
            _ => todo!(),
        };

        if let Some(callee) = module.get_function(&callee_name.to_string()) {
            let args: Vec<BasicMetadataValueEnum> = args
                .into_iter()
                .map(|arg| match arg.kind {
                    ExprKind::Literal(literal) => match literal.kind {
                        LiteralKind::String => {
                            Self::compile_string_literal(&context, &builder, &module, literal)
                                .as_basic_value_enum()
                                .into()
                        }
                        LiteralKind::Integer => {
                            Self::compile_integer_literal(&context, &builder, &module, literal)
                                .as_basic_value_enum()
                                .into()
                        }
                    },
                    ExprKind::Variable { name } => {
                        let (param_index, _) = caller_params
                            .into_iter()
                            .enumerate()
                            .find(|(_, param)| param.name == name)
                            .expect(&format!("Param '{}' not found.", name));

                        caller
                            .get_nth_param(param_index as u32)
                            .expect("Param not found")
                            .as_basic_value_enum()
                            .into()
                    }
                    ExprKind::Call { fun, args } => Self::compile_fn_call(
                        &context,
                        &builder,
                        &module,
                        caller,
                        caller_params,
                        fun,
                        args,
                    )
                    .unwrap()
                    .try_as_basic_value()
                    .unwrap_left()
                    .into(),
                })
                .collect::<Vec<_>>();

            Ok(builder.build_call(callee, args.as_slice(), "tmp"))
        } else {
            eprintln!("Function '{}' not found.", callee_name);
            Err(format!("Function '{}' not found.", callee_name))
        }
    }
}
