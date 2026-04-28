**Algorithm W: Hindley-Milner Type Inference Explained**

**Hindley-Milner Type Inference and Why It's Useful**

Hindley-Milner (HM) type inference is a classical type system that can automatically deduce the types of expressions in a program without any type annotations from the programmer[en.wikipedia.org](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#:~:text=Among%20HM%27s%20more%20notable%20properties,like%20those%20in%20%2084). This type system supports _parametric polymorphism_, meaning functions can be generic (e.g. an identity function works on values of any type). HM will infer the _most general type_ (principal type) for each expression that still makes the program well-typed[en.wikipedia.org](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#:~:text=Among%20HM%27s%20more%20notable%20properties,like%20those%20in%20%2084). This is extremely useful in functional programming languages like Haskell, OCaml and ML, where you can write code in an "untyped" style (no explicit type declarations) and let the compiler figure out the types automatically[en.wikipedia.org](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#:~:text=Among%20HM%27s%20more%20notable%20properties,like%20those%20in%20%2084). In practice, HM allows these languages to have strong static type checking _and_ generic abstractions without burdening the programmer with manual type annotations.

To understand the intuition: HM type inference _"formalizes the intuition that a type can be deduced by the functionality it supports."_ In other words, how a value is used in the code determines what its type must be[legacy-blog.akgupta.ca](https://legacy-blog.akgupta.ca/blog/2013/05/14/so-you-still-dont-understand-hindley-milner/#:~:text=%3E%20Functionally%20speaking%2C%20Hindley,by%20the%20functionality%20it%20supports). For example, if you pass a value to an addition function, the compiler infers that this value's type must be numeric. This ability to infer types from context makes programming more convenient (you write less boilerplate) while still catching type errors at compile time. Overall, Hindley-Milner's combination of flexibility (through polymorphic types) and safety (through compile-time checking) is a key reason languages like Haskell and OCaml can have zero-overhead abstraction with no explicit type declarations.

**Algorithm W and Its Role in HM Type Inference**

While HM can be described by a set of logical _typing rules_, in practice we need an algorithm to **compute** types. _Algorithm W_ is the standard algorithm for implementing Hindley-Milner type inference[en.wikipedia.org](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#:~:text=Among%20HM%27s%20more%20notable%20properties,like%20those%20in%20%2084). It was first described by Robin Milner in 1978 as part of the ML language's type system, and it efficiently infers types for expressions in an HM type system. The algorithm takes a given expression (and a type environment for free variables) and produces two things: (1) a **type** for the expression, and (2) a **substitution** capturing type information learned during inference (more on this soon). Algorithm W always finds the _most general_ type for the expression if a type exists, or reports a type error if none is possible[en.wikipedia.org](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#:~:text=Among%20HM%27s%20more%20notable%20properties,like%20those%20in%20%2084).

Algorithm W plays a crucial role in languages with HM type systems - it's essentially the type checker. Whenever you write a piece of code without type annotations, the compiler uses an Algorithm W variant to infer the types. It's known to be sound (if it infers a type, the type is actually valid for the expression) and complete (if an expression is typeable under HM, the algorithm will infer a type for it) with respect to the HM type rules. In summary, Algorithm W is the workhorse that takes the elegant theory of Hindley-Milner and applies it to real code, making type inference feasible on large codebases[en.wikipedia.org](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#:~:text=Among%20HM%27s%20more%20notable%20properties,like%20those%20in%20%2084).

**Key Concepts Underlying Algorithm W**

Before walking through the algorithm, let's clarify some core concepts: **constraint generation**, **unification**, **type environments (with polymorphic type schemes)**, and **substitutions**. These are the building blocks of how Algorithm W infers types.

**Constraint Generation (Type Equations)**

**Constraint generation** refers to the process of gathering requirements on types as we analyze an expression. As the algorithm traverses the program's abstract syntax tree, it generates type equations (constraints) that must hold for the program to be well-typed. For example, if we have a function application expression f(3), the algorithm introduces a constraint that the type of f must be a function type accepting an int. We might express that as: **type(f) = int → β**, for some fresh type variable β representing the result type. Similarly, if elsewhere f's result is used in a context that requires, say, a Boolean, we'd get another constraint β = bool. These kinds of _"forced equalities"_ between types arise from how sub-expressions are used[pages.cs.wisc.edu](https://pages.cs.wisc.edu/~horwitz/CS704-NOTES/5a.TYPES-PART2.html#:~:text=When%20we%20use%20our%20informal,forces%20t2%20to%20be%20int). In general, whenever an operation combines values (function call, if-then-else branches, etc.), the compiler formulates equations asserting that the involved types must align appropriately.

All the constraints collected from an expression must be satisfied simultaneously for the program to type-check. Initially, these constraints contain **type variables** (placeholders for unknown types). The job of the type inference algorithm is to find specific types for those type variables that satisfy all constraints. This is where **unification** comes in: it's the procedure that solves the constraints.

**Unification (Solving Type Constraints)**

**Unification** is the heart of Algorithm W's type inference[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Unification%20is%20a%20small%20amount,and%20for%20using%20its%20result). Unification takes two type expressions that are expected to be equal and finds a substitution that makes them identical (or reports failure if no solution exists). In simpler terms, unification answers the question: _"What type variable assignments make these two types match?"_[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Unification%20is%20a%20small%20amount,and%20for%20using%20its%20result). If no assignment can make the types the same (for example, one is int and the other is bool), then we have a **type error**[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Let%E2%80%99s%20look%20at%20a%20few,them%20be%20the%20same%20type).

During unification, we're allowed to **replace type variables** with other types in order to make the two sides align[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=So%20what%20is%20it%3F%20It,type%20variables%20we%20can%20change)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=The%20actual%20variable%20replacement%20happens,in%20this%20example). Some basic examples of unification behavior:

- Unifying identical concrete types (e.g. int vs int) succeeds with no changes (no substitution needed)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Let%E2%80%99s%20look%20at%20a%20few,them%20be%20the%20same%20type).
- Unifying a type variable with a concrete type (e.g. α vs bool) succeeds by substituting that variable with the concrete type (here we'd produce the substitution { α := bool })[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=The%20actual%20variable%20replacement%20happens,in%20this%20example).
- Unifying two distinct type variables (e.g. α vs β) will substitute one with the other (e.g. { α := β }, effectively saying those type variables represent the same unknown type)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=The%20actual%20variable%20replacement%20happens,in%20this%20example).
- Unifying more complex types, like function types, works structurally: for T1 → T2 unifying with U1 → U2, we must unify T1 with U1 _and_ unify T2 with U2 (all must succeed)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=OK%2C%20as%20promised%2C%20back%20to,bool). For instance, unifying int → α with β → bool will first unify the parameter types (int vs β, giving { β := int }), and then unify the return types (α vs bool, giving { α := bool }). Composing these, we get { β := int, α := bool }, which indeed makes int → α and β → bool both become int → bool[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=OK%2C%20as%20promised%2C%20back%20to,bool).

One important rule in unification is the **occurs check**: a type variable cannot be unified with a type that already contains that same variable. For example, attempting to unify α with α → bool fails, because if we tried to set α = α→bool, α would contain itself, leading to an "infinite type"[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Second%3A%20You%20aren%E2%80%99t%20allowed%20to,%E2%80%9D). The algorithm detects such cases and reports a type error rather than inferring an infinite type.

If unification succeeds, it produces a **substitution** (a mapping of type variables to types - see below) that represents a solution to the constraint. Importantly, unification in Algorithm W is designed to find the **most general unifier (MGU)**[pages.cs.wisc.edu](https://pages.cs.wisc.edu/~horwitz/CS704-NOTES/5a.TYPES-PART2.html#:~:text=Given%20two%20type%20expressions%2C%20t1,an%20S%20exists%2C%20else%20fail)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=possible%20unification%20could%20be%20%60,c%60%20for%20both). The MGU is the substitution that solves the constraints without arbitrarily making types more specific than necessary. In other words, it finds the _minimal_ assumptions to make the types match, yielding the most general solution. For example, if we need to unify α -> α with β -> γ, one could imagine forcing both α, β, γ to int as a trivial solution, but the most general unifier would be { β := α, γ := α }, resulting in the type α -> α (still generic)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=possible%20unification%20could%20be%20%60,c%60%20for%20both). This property ensures that the overall type inference yields principal (most general) types.

**Type Environments and Polymorphic Type Schemes**

A **type environment** (also called a context) is essentially a symbol table that tracks the types of variables in scope. In Hindley-Milner, the environment maps each program variable to a _type scheme_ rather than a simple type[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=the%20type%20of%20the%20variable,whether%20it%20is%20a%20function). A **type scheme** is a type with potentially some _universally quantified_ type variables. For example, we might record the identity function's type in the environment as id : ∀a. a -> a (in code we often write id : a -> a with the understanding that a is universally quantified)[en.wikipedia.org](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#:~:text=cons%C2%A0%3A%20forall%20a%20,a). The "∀a" means that id can be instantiated to any type a as needed. In contrast, a monomorphic type has no quantifiers (it's a concrete type or contains only specific type variables that are not quantified).

**Generalization:** When the algorithm infers a type for a let-bound expression, it will _generalize_ that type before adding it to the environment as a scheme. Generalization means figuring out which type variables in the inferred type can be safely generalized (turned into ∀-quantified variables)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=match%20at%20L489%20just%20copied,appear%20in%20the%20type%20environment)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=match%20at%20L529%20,scheme%20out%20of%20a%20type). The rule is that any type variables that _do not_ appear in the current environment (i.e. are not fixed by the surrounding context) are considered _generic_ and can be universally quantified[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=match%20at%20L489%20just%20copied,appear%20in%20the%20type%20environment)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=match%20at%20L529%20,scheme%20out%20of%20a%20type). For example, if we infer the type of a let-bound function id as α -> α (with α being a fresh type variable not coming from the environment), we generalize it to the scheme ∀α. α -> α before recording id in the environment[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=match%20at%20L529%20,scheme%20out%20of%20a%20type). This marks id as polymorphic. Later, each time we use id, we can instantiate it to a new type as needed.

**Instantiation:** When a variable is used in an expression, we must replace its generic type variables with fresh ones to avoid accidentally "coupling" different uses of the same polymorphic function. This process is called _instantiation_. For example, if id has scheme ∀a. a -> a in the environment, and we see it used in two different places, we instantiate a fresh type variable each time (say, a1 -> a1 for the first use and a2 -> a2 for the second use). This allows id to behave as if it had different types in each usage, which is how a single polymorphic definition can be applied to arguments of different types[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Both%20times%20we%20use%20the,appears)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=,type%20scheme%20produces%20a%20type). In algorithmic terms: to **lookup** a variable's type, we fetch its scheme from the environment and _instantiate_ a new copy of that type (with fresh type vars wherever the scheme had universally quantified ones)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=These%20aren%E2%80%99t%20bad%20either,doesn%E2%80%99t%20have%20anything%20for%20the). This instantiated type is then used in the inference process. (If a variable is not found in the environment, that means an unbound variable error.)

It's important to note that Hindley-Milner only allows **let-bound** variables to be truly polymorphic. Function parameters (lambda-bound variables) are treated as monomorphic within the body of the function. In other words, when inferring a lambda expression, we give its parameter a fresh type variable, but we do _not_ generalize it - the type variable represents one specific (unknown) type throughout that function body[en.wikipedia.org](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#:~:text=variables%20that%20are%20immediately%20bound,above%20example%20takes%20the%20form). Only after a value is bound with a top-level or let-binding do we generalize its type. This restriction (known as _let-polymorphism_) ensures type inference remains decidable[en.wikipedia.org](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#:~:text=variables%20that%20are%20immediately%20bound,above%20example%20takes%20the%20form). It means a function's argument cannot magically be used at two different types in the same function invocation; if needed, you can always wrap a polymorphic value in a let to achieve a similar effect. We will see this distinction in action when looking at examples of let-bound polymorphic functions.

**Substitutions and Compositions**

A **substitution** is a mapping from type variables to concrete types (or to other type variables)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Substitutions%20are%20a%20mapping%20from,int). For instance, { α := int, β := bool } is a substitution that replaces type variable α with int and β with bool. Applying a substitution to a type means systematically replacing all occurrences of those type variables with the corresponding types. For example, applying { α := (β -> β), γ := int } to the type α -> (β -> γ) yields (β -> β) -> (β -> int)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Substitutions%20are%20a%20mapping%20from,int). In the context of Algorithm W, substitutions represent knowledge we've gained about type variables by unifying constraints.

As the inference algorithm runs, it accumulates substitutions and _composes_ them together. If we have two substitutions S₁ and S₂ that arise at different points, composing them (written S₂ ∘ S₁) means we first apply S₁, then apply S₂ to the result[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=composing%20the%20substitutions%20,sub3). The result is a single combined substitution that reflects all the type refinements discovered so far. Substitution composition is crucial because the algorithm often infers information in one part of the expression (producing a substitution) and needs to ensure that information is consistently applied to the rest of the expression as it continues. For example, if we discovered somewhere that a type variable α must be int, we want that knowledge to be used in any other constraints involving α.

Algorithm W returns a substitution along with the inferred type for an expression precisely to convey this accumulated knowledge. You can think of the final substitution as part of the output: it tells you, for all the type variables that were introduced during inference, what concrete type they ended up being (or remain as type variables if they are truly generic)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=The%20function%20also%20returns%20two,besides%20just%20the%20expression). In many implementations, the final substitution is applied to the raw inferred type to produce a fully resolved principal type for the expression.

**Summary:** Constraint generation produces equations between types based on program structure; unification solves those equations by finding substitutions for type variables; the type environment stores polymorphic type schemes and requires instantiation on use and generalization on binding; and substitutions are composed throughout to build the overall type solution. With these concepts in mind, let's walk through how Algorithm W actually infers types for different kinds of expressions.

**How Algorithm W Infers Types (Walkthrough)**

At a high level, Algorithm W works by recursively traversing the abstract syntax tree of the program and inferring types for sub-expressions, using the concepts above. It considers each kind of expression syntax (variables, lambdas, applications, let-expressions, etc.) and applies a specific strategy for each. We will go through the main cases and see what the algorithm does:

- **Variable:** To infer the type of a variable x, the algorithm looks up x in the current type environment to get its type scheme (the type we assume for x based on what's in scope). It then **instantiates** that scheme to produce a fresh type. For example, if the environment has x : ∀a. a -> a, an instantiation might produce the type α -> α (with a fresh α) for this occurrence of x. The algorithm returns that type as the type of the variable, and it returns an empty substitution (since looking up a variable doesn't by itself constrain any new type variables)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=These%20aren%E2%80%99t%20bad%20either,doesn%E2%80%99t%20have%20anything%20for%20the). If x is not found in the environment, that's an error (using an undefined variable). Essentially, variables are the leaves of the type inference process: we rely on the environment for their types.
- **Lambda (Function Abstraction):** For a lambda expression (an anonymous function) like λx. e (sometimes written (x -> e)), the algorithm will assign a fresh type variable to the parameter x. We don't know x's type yet, so assume x : α for a new α. It extends the type environment with this assumption (mapping x to a _monomorphic_ scheme α, which we can think of as Scheme(\[\], α) - a scheme with no quantified variables, just α as-is)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Remember%20that%20functions%20have%20just,wrapped%20in%20a%20type%20scheme)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=modify%20the%20existing%20type%20environment,wrapped%20in%20a%20type%20scheme). Then the algorithm recursively infers the type of the function body e under this extended environment. Let's say this returns a substitution S₁ and a body type τ_body. Now, any constraints from the body might have refined the type of x (for example, if x was used in an arithmetic expression, S₁ might contain { α := int }). So we apply S₁ to α to get the possibly-updated parameter type[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Next%2C%20that%20substitution%20we%20just,to%20it%20returns%20it%20unchanged). Finally, the type of the lambda expression will be the function type α → τ_body (after applying the substitution to α). The algorithm returns this function type along with the substitution S₁ (since any information learned in the body's inference is still relevant outside the lambda)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Next%2C%20that%20substitution%20we%20just,to%20it%20returns%20it%20unchanged). In short, a lambda's type is inferred by inferring its result type under the assumption of a generic parameter type.
- **Function Application:** For an application e1 e2 (function e1 applied to argument e2), the algorithm needs to ensure that the function's parameter type matches the argument's type. It proceeds in multiple steps[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=When%20figuring%20out%20the%20type,that%2C%20we%E2%80%99ll%20have%20enough%20information)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=That%20was%20a%20lot%20of,the%20type%20of%20the%20expression):
  - **Fresh result type:** Introduce a fresh type variable, say β, to represent the _result type_ of this application (β will hold the type that e1 e2 evaluates to).
  - **Infer function type:** Recursively infer the type of e1. Suppose this yields substitution S₁ and type τ₁.
  - **Infer argument type:** Apply S₁ to the environment (to incorporate any knowledge gained so far) and then infer the type of e2, yielding substitution S₂ and type τ₂[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Right%20now%2C%20we%20know%20the,type1)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Step%204%3A%20Speaking%20of%20the,type2). We apply S₂ to τ₁ as well, obtaining an updated function type τ₁'. Now τ₁' is the type of e1 after accounting for all prior substitutions.
  - **Unify for application:** We know e1 is supposed to be a function taking an argument of type τ₂ and returning something of type β. So we form the constraint τ₁' = (τ₂ → β). The algorithm now calls **unification** on these two types[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=function%E2%80%99s%20return%20type%20as%20the,figure%20out%20the%20return%20type)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Step%207%3A%20Call%20the%20unification,type3). If unification succeeds, we get a substitution S₃ that makes τ₁' and τ₂ → β identical. In effect, this will fill in β with the actual return type. For example, if τ₁' turned out to be int → bool and τ₂ is int, unification will make β = bool (and if τ₁' was generic like α → α, unification would set α equal to τ₂ and β equal to τ₂ as well).
  - **Finalize:** We compose all the substitutions from this process (S_final = S₃ ∘ S₂ ∘ S₁) to get a single cumulative substitution[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Step%208%3A%20We%20are%20almost,sub3)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=composing%20the%20substitutions%20,sub3). The type of the application expression e1 e2 is then β with all those substitutions applied (often we just apply S_final to β). In practice, you can determine the type by looking at what β got unified to in S₃[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Step%207%3A%20Call%20the%20unification,type3)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=type%20of%20the%20function%20is,the%20type%20of%20the%20expression). Using a summary of this process: if e1 had type _t1 → t2_ and e2 had type _t3_, we unify _t1 → t2_ with _t3 → β_. This will enforce _t1 = t3_ (the argument types match) and give us a substitution where β is set to the return type. Thus, the result type of the application is whatever β was unified to. If no unifier can make τ₁ match a function type of τ₂ → β (for instance, if τ₁ was an int when we expected a function), then the application is a type error.
- **Let-Binding:** A let expression like let x = e1 in e2 is where polymorphism (generalization) comes into play. The algorithm handles let x = e1 in e2 in this way[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Step%201%3A%20Find%20the%20type,type1)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Let%E2%80%99s%20call%20the%20results%20,type2):
  - **Infer e1:** First, infer the type of e1 under the current environment. Suppose this gives substitution S₁ and type τ₁ for e1[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Step%201%3A%20Find%20the%20type,type1).
  - **Generalize:** Apply S₁ to τ₁ (and to the environment's types) to account for learned info, then **generalize** τ₁ with respect to the (updated) environment[jeremymikkola.comjeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Step%202%3A%20Create%20a%20new,type%20environment%20for%20anything%20else). Generalization will turn τ₁ into a type scheme σ (i.e. quantify any type variables in τ₁ that are not already fixed by the environment)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Step%202%3A%20Create%20a%20new,type%20environment%20for%20anything%20else). This σ becomes the _polymorphic type_ for x.
  - **Extend Environment:** Add x : σ to the type environment (for the scope of e2).
  - **Infer e2:** Now, infer the type of the body expression e2 under this extended environment (don't forget to also apply S₁ to the extended environment before inferring e2, to carry over any constraints learned from e1)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Step%204%3A%20Create%20,again%20in%20the%20second%20expression). This yields another substitution S₂ and result type τ₂ for e2.
  - **Finalize:** Compose the substitutions (S_final = S₂ ∘ S₁)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=The%20result%20of%20the%20function,sub2). The type of the entire let expression is τ₂ with that final substitution applied (typically we present τ₂ after applying S_final). The substitution is returned to reflect knowledge accumulated from both e1 and e2.

The critical part in the let-binding is step 2: by generalizing the type of e1 into a scheme, we allow x to be used polymorphically in e2. For example, consider:

let id = (\\x -> x) in (id 5, id True)

Here e1 is \\x->x, which infers type α -> α for some fresh α. Generalizing yields the scheme ∀α. α -> α for id. In the body e2 (which is the tuple (id 5, id True)), we instantiate id twice: once with α = Int for id 5, and once with α = Bool for id True. Both uses type-check, and the pair has type (Int, Bool). This is valid because id was generalized in the let-binding, allowing each use to have its own type[en.wikipedia.org](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#:~:text=let%20id%20%3D%20%CE%BB%20x,). In contrast, if id were a lambda parameter (not let-bound), it wouldn't be generalized and you couldn't apply it to both an Int and a Bool in the same scope[en.wikipedia.org](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#:~:text=variables%20that%20are%20immediately%20bound,above%20example%20takes%20the%20form) (the algorithm would try to unify those uses to a single type and fail). Thus, let-binding is what enables **parametric polymorphism** in HM: each let-bound value's type variables can be universally quantified and give rise to independent instantiations on each usage.

**Examples of Algorithm W in Action**

Let's walk through a couple of simple examples to illustrate how Algorithm W infers types:

- **Example 1: Polymorphic Identity Function** - Consider the identity function defined as id = \\x -> x. We want to infer id's type. The algorithm treats this as a let-binding (if at top level) or simply a lambda. Inside the lambda, x gets a fresh type variable (say α). The body of the function is x itself, which of course has type α. Therefore, the function's body is inferred to have type α, and no additional constraints arose. The lambda abstraction then produces the function type α → α for id. Since α was unconstrained (it could be anything), we generalize over α. Thus id is assigned the scheme ∀α. α -> α[en.wikipedia.org](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#:~:text=cons%C2%A0%3A%20forall%20a%20,a). In other words, the algorithm infers that id can accept any type and return the same type. If we use id in an expression (e.g. id 42 or id "hello"), the instantiation step will create a fresh type variable for that usage and unify it with the argument's type (so id 42 will yield type Int, id "hello" yields String, etc., all consistent with id : ∀α. α->α).
- **Example 2: Inferring a Function with Constraints** - Suppose we have inc = \\y -> y + 1. In a language like Haskell or ML, the (+) operator typically has a type like Int -> Int -> Int (for simplicity, assume it only works on Ints). During inference, for the lambda \\y -> y + 1, the algorithm gives y a fresh type variable (α). Then it sees the expression y + 1. The operator + expects two Ints and returns an Int, so y (left operand) must be Int. This generates a constraint α = Int, which unification will eagerly solve by producing substitution { α := Int }. Thus, the body's type is inferred as Int (the result of the addition). The lambda then has type Int -> Int (since α got replaced with Int). There are no remaining unknown type variables to generalize here (α is now fixed to Int), so inc's type is monomorphic Int -> Int. If we try to use inc on a non-integer, the inference will fail because it will eventually attempt to unify that argument's type with Int and report a mismatch.
- **Example 3: Let-Bound Polymorphism vs. Monomorphism** - To reinforce the earlier point about let-bound polymorphism, consider these two scenarios:
  - We create a polymorphic function using a let:
  - let makePair = (\\f -> (f 3, f "hi")) in makePair

In this (pseudo-ML) code, makePair is a function that takes f and returns a pair by calling f on an int and on a string. Type inference for makePair will find a constraint that f must accept an int and a string. This is only possible if f itself is polymorphic (can accept multiple types). However, f is a lambda parameter here, not a let-bound variable - therefore it is monomorphic in this context[en.wikipedia.org](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#:~:text=variables%20that%20are%20immediately%20bound,above%20example%20takes%20the%20form). The inference will fail (indeed, this corresponds to a well-known result that HM type inference cannot infer types for such a scenario - it requires higher-rank polymorphism).

- 1. By contrast, if we restructure the code to let-bind a polymorphic helper, it works:
  - let id = (\\x -> x) in (id 3, id "hi")

As discussed, id is inferred as ∀α. α->α in the let. In the tuple, id 3 uses an instance with α = Int, and id "hi" uses an instance with α = String. The pair's type comes out as (Int, String) with no issues, because id was allowed to be polymorphic (being let-bound)[en.wikipedia.org](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#:~:text=let%20id%20%3D%20%CE%BB%20x,).

In summary, these examples show Algorithm W in action: introducing fresh type variables, generating and solving constraints (like α = Int from y+1), and generalizing let-bound results to achieve polymorphism. They also show how a type error can occur when constraints conflict (e.g. trying to use one function on two different types without letting it be polymorphic).

**Implementing Algorithm W: Tips and Pseudocode**

Implementing Algorithm W involves encoding the above logic in code. Typically, you would represent types, substitutions, and the environment with appropriate data structures, and then write a recursive inference function. Here are some guiding points:

- **Type Representation:** Define a data type for types that can include: base types (e.g. Int, Bool), type variables, and function types. For example, in a pseudo-code style:
- Type = IntType
- | BoolType
- | TypeVar(name)
- | FuncType(Type, Type)

In our earlier discussion, a type variable like α might be represented as TypeVar("α"). A function type t1 → t2 could be represented as FuncType(t1, t2). (If your implementation language supports generics or unions, you'll model it accordingly.)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=In%20addition%20to%20concrete%20types,whatever%20that%20ends%20up%20being)

- **Type Environment:** Represent the environment as a mapping from variable names to _type schemes_. A type scheme can be a structure holding a list of universally quantified type variables and a type (often written as Scheme(\[t1, t2, ...\], type)). For instance, after generalization the identity function might be stored as ("id" -> Scheme(\[α\], α -> α))[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=the%20type%20of%20the%20variable,whether%20it%20is%20a%20function). The environment operations you need are: lookup a variable and instantiate its scheme (produce a fresh type), and extend the environment with a new binding (for let-expressions or function parameters).
- **Substitution:** You can represent a substitution as a map from type variable names to types (e.g. { "α": IntType, "β": BoolType }). You'll need functions to apply a substitution to a type (recursively replace any type vars present) and to compose substitutions[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Substitutions%20are%20a%20mapping%20from,int)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=composing%20the%20substitutions%20,sub3). Often, composition is implemented by applying one substitution to the result of another and merging the mappings.
- **Unification Function:** Implement unification as a function that takes two types and produces a substitution (or an error if unification fails). This is typically a recursive algorithm:
  - If one type is a type variable and the other is anything: check the occurs condition (the type variable must not appear inside the other type)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=%60,the%20types%20be%20the%20same). If it passes, substitute the variable with the other type (e.g. return { α := otherType }). If the other side is the _same_ type variable (α vs α), no substitution is needed (return empty substitution)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=There%20are%20two%20special%20cases,unification%20that%20I%20should%20mention).
  - If both types are function types, unify their parts: unify the left parts and unify the right parts, then compose the resulting substitutions.
  - If both are the same concrete type (Int vs Int, Bool vs Bool, etc.), succeed with no substitution.
  - Any other combination (mismatched type constructors like Int vs Bool, or Int vs function type) fails (type error).

As you unify, you'll apply accumulated substitutions to types before proceeding, to always work on the most up-to-date forms. This function will construct the most general unifier as discussed.

- **Inference Function (Algorithm W):** Write a recursive function infer(env, expr) that returns a pair (substitution, type) for the given expression. Using pseudocode, Algorithm W can be structured as follows:

function infer(env, expr):

match expr:

case Variable(x):

// Look up and instantiate

let Scheme(typeVars, typeBody) = env\[x\]

let instantiatedType = freshCopyOf(typeBody, typeVars)

return (∅, instantiatedType) // no new constraints, type as is

case Lambda(x, bodyExpr):

// Create fresh type var for parameter

freshTypeVar α

// Extend environment with x: α (monomorphic scheme)

let env1 = env + (x -> Scheme(\[\], α))

// Infer body

(S1, τ_body) = infer(env1, bodyExpr)

// Apply substitution to the param type in case it was constrained

let τ_param = S1(α)

// The lambda's type is τ_param -> τ_body

return (S1, FuncType(τ_param, τ_body))

case Application(funExpr, argExpr):

freshTypeVar β // for result type

// Infer function and argument sub-expressions

(S1, τ_fun) = infer(env, funExpr)

(S2, τ_arg) = infer(S1(env), argExpr) // apply S1 to env for arg

// Apply S2 to the function's type (it may involve vars refined by arg)

let τ_fun2 = S2(τ_fun)

// Unify function's type with (argument type -> β)

let S3 = unify(τ_fun2, FuncType(τ_arg, β))

// Compose all substitutions S_final = S3 ∘ S2 ∘ S1

let S_final = S3 ∘ S2 ∘ S1

// Result type is S_final(β)

return (S_final, S_final(β))

case Let(name, e1, e2):

// Infer type of e1

(S1, τ1) = infer(env, e1)

// Apply S1 to env and τ1

let env1 = S1(env)

let τ1_sub = S1(τ1)

// Generalize τ1_sub relative to env1

let scheme = generalize(env1, τ1_sub)

// Extend env1 with the new polymorphic binding

let env2 = env1 + (name -> scheme)

// Infer type of e2 with extended env

(S2, τ2) = infer(env2, e2)

// Compose substitutions

let S_final = S2 ∘ S1

return (S_final, S_final(τ2))

This pseudocode outlines the core of Algorithm W. Each case corresponds to what we described in the walkthrough: for variables we instantiate, for lambdas we introduce a new type variable, for applications we enforce a function type via unification, and for let-expressions we generalize the first sub-expression's type. The order of applying and composing substitutions ensures that information flows from sub-expressions back up the syntax tree[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Step%208%3A%20We%20are%20almost,sub3)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=The%20result%20of%20the%20function,sub2). In an actual implementation, you would also handle literals (constants) as a base case (e.g. numeric or boolean literal nodes just produce type Int or Bool with no substitution)[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Literals.%20e.g.%20).

- **Data Structures:** A common approach is to implement type variables as references or indices that can be unified imperatively (union-find or mutable substitution maps), but a purely functional approach as sketched above works too by returning substitutions explicitly. A convenient trick is giving each fresh type variable a unique identifier (like a running counter that produces t0, t1, t2, etc.), which makes it easier to implement freshTypeVar and ensure each new variable is distinct.

By following this algorithm, one can build a type inferencer for a small functional language. Each piece - environment management, unification, substitution application - can be tested individually. Once it's all put together, you'll have a system that can infer types and catch type errors in code that didn't have any explicit type annotations. This is essentially how compilers for ML, Haskell, and similar languages perform type inference using Algorithm W[jeremymikkola.com](https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html#:~:text=Function%20abstractions%20involved%20creating%20a,environment%2C%20and%20applying%20a%20substitution). With careful implementation (and possibly some optimizations), even complex programs with many let-bound polymorphic functions can be type-checked efficiently using this algorithm.
