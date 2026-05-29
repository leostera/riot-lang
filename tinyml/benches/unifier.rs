use criterion::{Criterion, criterion_group, criterion_main};
use tinyml::checker::{builtin, state::State, tst::Type, unifier::Unifier};

fn primitive_equality(c: &mut Criterion) {
    c.bench_function("unifier primitive equality", |b| {
        b.iter(|| {
            let mut state = State::new();
            let mut unifier = Unifier::new(&mut state);
            unifier
                .unify(&builtin::i32(), &builtin::i32())
                .expect("unify");
        })
    });
}

fn nested_structures(c: &mut Criterion) {
    c.bench_function("unifier nested structures", |b| {
        b.iter(|| {
            let mut state = State::new();
            let ty = Type::Arrow {
                parameter: Box::new(Type::Tuple(vec![builtin::i32(), builtin::bool()])),
                result: Box::new(builtin::string()),
            };
            let mut unifier = Unifier::new(&mut state);
            unifier.unify(&ty, &ty).expect("unify");
        })
    });
}

fn variable_chain_resolve(c: &mut Criterion) {
    c.bench_function("unifier variable chain resolve", |b| {
        b.iter(|| {
            let mut state = State::new();
            let a = state.fresh_var();
            let b = state.fresh_var();
            let c = state.fresh_var();
            let mut unifier = Unifier::new(&mut state);
            unifier.unify(&a, &b).expect("a b");
            unifier.unify(&b, &c).expect("b c");
            unifier.unify(&c, &builtin::unit()).expect("c unit");
            unifier.resolve(&a).expect("resolve")
        })
    });
}

fn occurs_check_failure(c: &mut Criterion) {
    c.bench_function("unifier occurs check failure", |b| {
        b.iter(|| {
            let mut state = State::new();
            let var = state.fresh_var();
            let mut unifier = Unifier::new(&mut state);
            let _ = unifier.unify(&var, &Type::Tuple(vec![var.clone()]));
        })
    });
}

criterion_group!(
    benches,
    primitive_equality,
    nested_structures,
    variable_chain_resolve,
    occurs_check_failure
);
criterion_main!(benches);
