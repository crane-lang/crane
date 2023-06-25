use std::collections::HashMap;
use std::process::Command;
use std::sync::Arc;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::passes::PassManager;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
};
use inkwell::types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType};
use inkwell::values::{
    BasicValue, BasicValueEnum, CallSiteValue, FunctionValue, GlobalValue, IntValue, PointerValue,
};
use inkwell::{AddressSpace, OptimizationLevel};
use smol_str::SmolStr;
use thin_vec::{thin_vec, ThinVec};

use crate::ast::{
    TyExpr, TyExprKind, TyFnParam, TyIntegerLiteral, TyItem, TyItemKind, TyLiteralKind,
    TyLocalKind, TyModule, TyPackage, TyPath, TyPathSegment, TyStmtKind, TyUint,
};
use crate::typer::Type;

pub struct NativeBackend<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    fpm: PassManager<FunctionValue<'ctx>>,
}

impl<'ctx> NativeBackend<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("main");
        let builder = context.create_builder();

        let fpm = PassManager::create(&module);

        Self {
            context,
            module,
            builder,
            fpm,
        }
    }

    pub fn compile(&self, package: TyPackage) {
        Target::initialize_aarch64(&InitializationConfig::default());

        let opt = OptimizationLevel::Default;
        let reloc = RelocMode::Default;
        let model = CodeModel::Default;

        let target = Target::from_name("aarch64").expect("Failed to parse target");

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

        self.fpm.add_instruction_combining_pass();

        self.fpm.initialize();

        // Define `puts`.
        let puts = {
            let fn_name = "puts";

            let i8_type = self.context.i8_type();
            let i32_type = self.context.i32_type();
            let fn_type = i32_type.fn_type(
                &[i8_type
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum()
                    .into()],
                false,
            );

            let puts = self
                .module
                .add_function(fn_name, fn_type, Some(Linkage::External));

            self.verify_fn(fn_name, &puts).unwrap();

            fn_name
        };

        // Define `sprintf`.
        let sprintf = {
            let fn_name = "sprintf";

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

            let sprintf = self
                .module
                .add_function(fn_name, fn_type, Some(Linkage::External));

            self.verify_fn(fn_name, &sprintf).unwrap();

            fn_name
        };

        // Define `printf`.
        let printf = {
            let fn_name = "printf";

            let i8_type = self.context.i8_type();
            let i32_type = self.context.i32_type();

            let fn_type = i32_type.fn_type(
                &[i8_type
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum()
                    .into()],
                true,
            );

            let printf = self
                .module
                .add_function(fn_name, fn_type, Some(Linkage::External));

            self.verify_fn(fn_name, &printf).unwrap();

            fn_name
        };

        // Define `std::io::print`.
        {
            let fn_name = "std::io::print";

            let i8_type = self.context.i8_type();

            let fn_type = self.context.void_type().fn_type(
                &[i8_type
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum()
                    .into()],
                false,
            );

            let fn_value = self.module.add_function(fn_name, fn_type, None);

            let value_param = fn_value.get_first_param().unwrap();

            let entry = self.context.append_basic_block(fn_value, "entry");

            self.builder.position_at_end(entry);

            let template = b"%1$s";

            let i8_type = self.context.i8_type();
            let i8_array_type = i8_type.array_type(template.len() as u32 + 1);

            let template = self.context.const_string(template, true);

            let global = self
                .module
                .add_global(i8_array_type, None, "print_template");
            global.set_linkage(Linkage::Internal);
            global.set_constant(true);
            global.set_initializer(&template);

            if let Some(callee) = self.module.get_function(printf) {
                self.builder.build_call(
                    callee,
                    &[global.as_basic_value_enum().into(), value_param.into()],
                    "tmp",
                );
            } else {
                eprintln!("Function '{}' not found.", printf);
            }

            self.builder.build_return(None);

            self.verify_fn(fn_name, &fn_value).unwrap();
        }

        // Define `std::io::println`.
        {
            let fn_name = "std::io::println";

            let i8_type = self.context.i8_type();

            let fn_type = self.context.void_type().fn_type(
                &[i8_type
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum()
                    .into()],
                false,
            );

            let fn_value = self.module.add_function(fn_name, fn_type, None);

            let value_param = fn_value.get_first_param().unwrap();

            let entry = self.context.append_basic_block(fn_value, "entry");

            self.builder.position_at_end(entry);

            if let Some(callee) = self.module.get_function(puts) {
                self.builder
                    .build_call(callee, &[value_param.into()], "tmp");
            } else {
                eprintln!("Function '{}' not found.", puts);
            }

            self.builder.build_return(None);

            self.verify_fn(fn_name, &fn_value).unwrap();
        }

        // Define `std::int::int_add`.
        {
            let fn_name = "std::int::int_add";

            let i64_type = self.context.i64_type();

            let fn_type = self.context.i64_type().fn_type(
                &[
                    i64_type.as_basic_type_enum().into(),
                    i64_type.as_basic_type_enum().into(),
                ],
                false,
            );

            let fn_value = self.module.add_function(fn_name, fn_type, None);

            let lhs_param = fn_value.get_first_param().unwrap().into_int_value();
            let rhs_param = fn_value.get_nth_param(1).unwrap().into_int_value();

            let entry = self.context.append_basic_block(fn_value, "entry");

            self.builder.position_at_end(entry);

            let sum = self.builder.build_int_add(lhs_param, rhs_param, "sum");

            self.builder.build_return(Some(&sum));

            self.verify_fn(fn_name, &fn_value).unwrap();
        }

        // Define `std::int::int_to_string`.
        {
            let fn_name = "std::int::int_to_string";

            let i64_type = self.context.i64_type();
            let i8_type = self.context.i8_type();
            let i8_ptr_type = i8_type.ptr_type(AddressSpace::default());

            let fn_type = i8_ptr_type.fn_type(&[i64_type.as_basic_type_enum().into()], false);

            let fn_value = self.module.add_function(fn_name, fn_type, None);

            let int_value = fn_value.get_first_param().unwrap().into_int_value();

            let entry = self.context.append_basic_block(fn_value, "entry");

            self.builder.position_at_end(entry);

            let buffer = self
                .builder
                .build_malloc(i8_ptr_type, "buffer")
                .expect("Failed to allocate `int_to_string` buffer.");

            let template = b"%1$d";

            let i8_type = self.context.i8_type();
            let i8_array_type = i8_type.array_type(template.len() as u32 + 1);

            let template = self.context.const_string(template, true);

            let global = self
                .module
                .add_global(i8_array_type, None, "int_to_string_template");
            global.set_linkage(Linkage::Internal);
            global.set_constant(true);
            global.set_initializer(&template);

            if let Some(callee) = self.module.get_function(sprintf) {
                self.builder.build_call(
                    callee,
                    &[
                        buffer.into(),
                        global.as_basic_value_enum().into(),
                        int_value.into(),
                    ],
                    "tmp",
                );
            } else {
                panic!("Function '{}' not found.", sprintf);
            }

            self.builder.build_return(Some(&buffer));

            self.verify_fn(fn_name, &fn_value).unwrap();
        }

        for item in package
            .modules
            .into_iter()
            .flat_map(|module| module.items)
            // HACK: Reverse the items so we define the helper functions before `main`.
            // This should be replaced with a call graph.
            .rev()
        {
            self.compile_item(&item);
        }

        self.module
            .print_to_file("build/main.ll")
            .expect("Failed to emit main.ll");

        let buffer = target_machine
            .write_to_memory_buffer(&self.module, FileType::Object)
            .expect("Failed to write to buffer");

        use std::io::Write;

        let mut outfile = std::fs::File::create("build/main.o").unwrap();

        outfile.write_all(buffer.as_slice()).unwrap();

        let bitcode = self.module.write_bitcode_to_memory();

        outfile.write_all(bitcode.as_slice()).unwrap();

        let exit_status = Command::new("clang")
            .args(["-o", "build/main", "build/main.o"])
            .status()
            .expect("Failed to build with clang");

        println!("clang exited with {}", exit_status);
    }

    fn verify_fn(&self, fn_name: &str, fn_value: &FunctionValue) -> Result<(), String> {
        if fn_value.verify(true) {
            self.fpm.run_on(fn_value);

            Ok(())
        } else {
            Err(format!("`{}` not verified", fn_name))
        }
    }

    fn compile_module(&self, ty_module: &TyModule) {
        for item in &ty_module.items {
            self.compile_item(item);
        }
    }

    fn to_llvm_type(&self, ty: Arc<Type>) -> AnyTypeEnum<'ctx> {
        match &*ty {
            Type::Fn {
                args: params,
                return_ty,
            } => {
                let params = params
                    .iter()
                    .map(|param| self.to_llvm_type(param.clone()))
                    .map(any_type_to_basic_metadata_type)
                    .collect::<Vec<_>>();

                let return_ty = self.to_llvm_type(return_ty.clone());

                AnyTypeEnum::FunctionType(match return_ty {
                    AnyTypeEnum::IntType(int_type) => int_type.fn_type(&params, false),
                    _ => panic!(""),
                })
            }
            Type::UserDefined { module, name } => match (module.as_ref(), name.as_ref()) {
                ("std::prelude", "String") => self
                    .context
                    .i8_type()
                    .ptr_type(AddressSpace::default())
                    .as_any_type_enum(),
                ("std::prelude", "Uint64") => self.context.i64_type().as_any_type_enum(),
                (module, name) => {
                    panic!("Unknown function parameter type: {}::{}", module, name)
                }
            },
        }
    }

    fn compile_item(&self, item: &TyItem) {
        match &item.kind {
            TyItemKind::Use => {}
            TyItemKind::Fn(fun) => {
                let params = fun
                    .params
                    .iter()
                    .map(|param| {
                        let param_type = self.to_llvm_type(param.ty.clone());

                        any_type_to_basic_metadata_type(param_type)
                    })
                    .collect::<Vec<_>>();

                let fn_type = match &*fun.return_ty {
                    Type::UserDefined { module, name } => match (module.as_str(), name.as_str()) {
                        ("std::prelude", "()") => self.context.void_type().fn_type(&params, false),
                        ("std::prelude", "Uint64") => {
                            self.context.i64_type().fn_type(&params, false)
                        }
                        ("std::prelude", "String") => self
                            .context
                            .i8_type()
                            .ptr_type(AddressSpace::default())
                            .fn_type(&params, false),
                        (module, name) => panic!("Unknown type {}::{}", module, name),
                    },
                    Type::Fn {
                        args: _,
                        return_ty: _,
                    } => todo!(),
                };

                let is_main_fn = item.name.name == "main";

                let fn_type = if is_main_fn {
                    self.context.i32_type().fn_type(&params, false)
                } else {
                    fn_type
                };

                let fn_value = self
                    .module
                    .add_function(&fun.path.to_string(), fn_type, None);

                for (index, param_value) in fn_value.get_param_iter().enumerate() {
                    if let Some(param) = fun.params.get(index) {
                        param_value.set_name(&param.name.to_string());
                    }
                }

                let entry = self.context.append_basic_block(fn_value, "entry");

                self.builder.position_at_end(entry);

                let mut locals = HashMap::new();

                let mut last_stmt: Option<BasicValueEnum> = None;

                for stmt in &fun.body {
                    match &stmt.kind {
                        TyStmtKind::Local(local) => {
                            let ty = local.ty.as_ref().unwrap_or_else(|| {
                                panic!("No type for `let` binding `{}`.", local.name)
                            });

                            let ty = match &*ty.clone() {
                                Type::UserDefined { module, name } => {
                                    match (module.as_str(), name.as_str()) {
                                        ("std::prelude", "()") => todo!(),
                                        ("std::prelude", "Uint64") => {
                                            self.context.i64_type().as_basic_type_enum()
                                        }
                                        ("std::prelude", "String") => self
                                            .context
                                            .i8_type()
                                            .ptr_type(AddressSpace::default())
                                            .as_basic_type_enum(),
                                        (module, name) => {
                                            panic!("Unknown type {}::{}", module, name)
                                        }
                                    }
                                }
                                Type::Fn {
                                    args: _,
                                    return_ty: _,
                                } => todo!(),
                            };

                            let local_ptr = self.builder.build_alloca(ty, &local.name.to_string());

                            let value = match &local.kind {
                                TyLocalKind::Decl => None,
                                TyLocalKind::Init(init) => self.compile_expr(
                                    &fun.params,
                                    &fn_value,
                                    &locals,
                                    *init.clone(),
                                ),
                            }
                            .unwrap_or_else(|| {
                                panic!(
                                    "`let` binding `{}` does not have an initializer.",
                                    local.name
                                )
                            });

                            self.builder.build_store(local_ptr, value);

                            let local_path = TyPath {
                                segments: thin_vec![TyPathSegment {
                                    ident: local.name.clone()
                                }],
                                span: local.name.span,
                            };

                            locals.insert(local_path, local_ptr);
                        }
                        TyStmtKind::Expr(expr) => {
                            last_stmt =
                                self.compile_expr(&fun.params, &fn_value, &locals, *expr.clone());
                        }
                        TyStmtKind::Item(_item) => todo!(),
                    }
                }

                if is_main_fn {
                    self.builder
                        .build_return(Some(&self.context.i32_type().const_int(0, false)));
                } else if let Some(last_stmt) = last_stmt {
                    self.builder.build_return(Some(&last_stmt));
                } else {
                    self.builder.build_return(None);
                }

                self.verify_fn(&item.name.to_string(), &fn_value).unwrap();
            }
            TyItemKind::Struct(_) => {}
            TyItemKind::Union(_) => {}
            TyItemKind::Module(ty_module) => {
                self.compile_module(&ty_module);
            }
        }
    }

    fn compile_expr(
        &self,
        fn_params: &ThinVec<TyFnParam>,
        fn_value: &FunctionValue<'ctx>,
        locals: &HashMap<TyPath, PointerValue<'ctx>>,
        expr: TyExpr,
    ) -> Option<BasicValueEnum<'ctx>> {
        match expr.kind {
            TyExprKind::Literal(literal) => match literal.kind {
                TyLiteralKind::String(literal) => {
                    Some(self.compile_string_literal(literal).as_basic_value_enum())
                }
                TyLiteralKind::Integer(literal) => {
                    Some(self.compile_integer_literal(literal).as_basic_value_enum())
                }
            },
            TyExprKind::Variable(_) => todo!(),
            TyExprKind::Call { fun, args } => self
                .compile_fn_call(fn_value, fn_params, fun.clone(), args, locals)
                .unwrap_or_else(|_| panic!("Failed to compile function call: {:?}", fun))
                .try_as_basic_value()
                .either(Some, |_| None),
        }
    }

    fn compile_string_literal(&self, literal: SmolStr) -> GlobalValue<'ctx> {
        // Unquote the string literal.
        let value = {
            let mut chars = literal.chars();
            chars.next();
            chars.next_back();
            chars.as_str()
        };

        let value = value.as_bytes();

        let i8_type = self.context.i8_type();
        let i8_array_type = i8_type.array_type(value.len() as u32 + 1);

        let string = self.context.const_string(value, true);

        let global = self.module.add_global(i8_array_type, None, "string_lit");
        global.set_linkage(Linkage::Internal);
        global.set_constant(true);
        global.set_initializer(&string);

        global
    }

    fn compile_integer_literal(&self, literal: TyIntegerLiteral) -> IntValue<'ctx> {
        let (int_value, int_type) = match literal {
            TyIntegerLiteral::Unsigned(value, TyUint::Uint64) => {
                (value as u64, self.context.i64_type())
            }
        };

        int_type.const_int(int_value, false)
    }

    fn compile_fn_call(
        &self,
        caller: &FunctionValue<'ctx>,
        caller_params: &ThinVec<TyFnParam>,
        fun: Box<TyExpr>,
        args: ThinVec<Box<TyExpr>>,
        locals: &HashMap<TyPath, PointerValue<'ctx>>,
    ) -> Result<CallSiteValue<'ctx>, String> {
        let callee_name = match fun.kind {
            TyExprKind::Variable(path) => path,
            _ => todo!(),
        };

        if let Some((param_index, callee)) = caller_params
            .iter()
            .enumerate()
            .find(|(_, param)| param.name.name == callee_name.to_string())
        {
            let function_type = self.to_llvm_type(callee.ty.clone()).into_function_type();

            let function_ptr = caller
                .get_nth_param(param_index as u32)
                .unwrap()
                .into_pointer_value();

            let args = args
                .into_iter()
                .map(|arg| match arg.kind {
                    TyExprKind::Literal(literal) => match literal.kind {
                        TyLiteralKind::String(literal) => self
                            .compile_string_literal(literal)
                            .as_basic_value_enum()
                            .into(),
                        TyLiteralKind::Integer(literal) => self
                            .compile_integer_literal(literal)
                            .as_basic_value_enum()
                            .into(),
                    },
                    TyExprKind::Variable(path) => {
                        let param = caller_params
                            .into_iter()
                            .enumerate()
                            .find(|(_, param)| {
                                Some(param.name.clone())
                                    == path.segments.last().map(|segment| segment.ident.clone())
                            })
                            .and_then(|(param_index, _)| caller.get_nth_param(param_index as u32));

                        param.unwrap().into()
                    }
                    TyExprKind::Call { fun, args } => self
                        .compile_fn_call(caller, caller_params, fun, args, locals)
                        .unwrap()
                        .try_as_basic_value()
                        .unwrap_left()
                        .into(),
                })
                .collect::<Vec<_>>();

            return Ok(self.builder.build_indirect_call(
                function_type,
                function_ptr,
                &args,
                &callee.name.name,
            ));
        }

        if let Some(callee) = self.module.get_function(&callee_name.to_string()) {
            let args = args
                .into_iter()
                .enumerate()
                .map(|(arg_index, arg)| match arg.kind {
                    TyExprKind::Literal(literal) => match literal.kind {
                        TyLiteralKind::String(literal) => self
                            .compile_string_literal(literal)
                            .as_basic_value_enum()
                            .into(),
                        TyLiteralKind::Integer(literal) => self
                            .compile_integer_literal(literal)
                            .as_basic_value_enum()
                            .into(),
                    },
                    TyExprKind::Variable(path) => {
                        let param = caller_params
                            .into_iter()
                            .enumerate()
                            .find(|(_, param)| {
                                Some(param.name.clone())
                                    == path.segments.last().map(|segment| segment.ident.clone())
                            })
                            .and_then(|(param_index, _)| caller.get_nth_param(param_index as u32));

                        let callee_param =
                            callee.get_nth_param(arg_index as u32).unwrap_or_else(|| {
                                panic!("No param for `{callee_name}` found at index {arg_index}");
                            });

                        let variable = param
                            .or_else(|| {
                                locals.get(&path).map(|local| {
                                    self.builder
                                        .build_load(callee_param.get_type(), *local, "load")
                                })
                            })
                            .or_else(|| {
                                self.module.get_function(&path.to_string()).map(|function| {
                                    function
                                        .as_global_value()
                                        .as_pointer_value()
                                        .as_basic_value_enum()
                                })
                            })
                            .unwrap_or_else(|| panic!("Variable `{}` not found.", path));

                        variable.into()
                    }
                    TyExprKind::Call { fun, args } => self
                        .compile_fn_call(caller, caller_params, fun, args, locals)
                        .unwrap()
                        .try_as_basic_value()
                        .unwrap_left()
                        .into(),
                })
                .collect::<Vec<_>>();

            Ok(self.builder.build_call(callee, args.as_slice(), "tmp"))
        } else {
            eprintln!("Function '{}' not found.", callee_name);
            Err(format!("Function '{}' not found.", callee_name))
        }
    }
}

fn any_type_to_basic_metadata_type<'ctx>(
    any_type: AnyTypeEnum<'ctx>,
) -> BasicMetadataTypeEnum<'ctx> {
    match any_type {
        AnyTypeEnum::ArrayType(array_type) => BasicMetadataTypeEnum::ArrayType(array_type),
        AnyTypeEnum::FloatType(float_type) => BasicMetadataTypeEnum::FloatType(float_type),
        AnyTypeEnum::IntType(int_type) => BasicMetadataTypeEnum::IntType(int_type),
        AnyTypeEnum::PointerType(ptr_type) => BasicMetadataTypeEnum::PointerType(ptr_type),
        AnyTypeEnum::StructType(struct_type) => BasicMetadataTypeEnum::StructType(struct_type),
        AnyTypeEnum::VectorType(vector_type) => BasicMetadataTypeEnum::VectorType(vector_type),
        AnyTypeEnum::VoidType(_) => todo!(),
        AnyTypeEnum::FunctionType(function_type) => {
            BasicMetadataTypeEnum::PointerType(function_type.ptr_type(AddressSpace::default()))
        }
    }
}
