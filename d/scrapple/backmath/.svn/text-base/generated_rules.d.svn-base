template TypeOfAdd(T, V)
{
	static if( /+ (+ (/> K (-> H X)) (/> F (-> E X))) -> (/> (+ K F) (-> (/ (+ (* H K) (* E F)) (+ K F)) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpAdd!(T.LHS, V.LHS), OpSubA!(OpDiv!(OpAdd!(OpMul!(T.RHS.LHS, T.LHS), OpMul!(V.RHS.LHS, V.LHS)), OpAdd!(T.LHS, V.LHS)), T.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (/> K (-> H X)) (*> F (-> E X))) -> (/> (+ K (/ 1 F)) (-> (/ (+ (* H K) (/ E F)) (+ K (/ 1 F))) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpAdd!(T.LHS, OpDiv!(Value!(1), V.LHS)), OpSubA!(OpDiv!(OpAdd!(OpMul!(T.RHS.LHS, T.LHS), OpDiv!(V.RHS.LHS, V.LHS)), OpAdd!(T.LHS, OpDiv!(Value!(1), V.LHS))), T.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (/> K (-> H X)) (/> E X)) -> (/> (+ K E) (-> (/ (* H K) (+ K E)) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS)
		) 
		alias OpDivA!(OpAdd!(T.LHS, V.LHS), OpSubA!(OpDiv!(OpMul!(T.RHS.LHS, T.LHS), OpAdd!(T.LHS, V.LHS)), T.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (/> K (-> H X)) (*> E X)) -> (/> (+ K (/ 1 E)) (-> (/ (* H K) (+ K (/ 1 E))) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS)
		) 
		alias OpDivA!(OpAdd!(T.LHS, OpDiv!(Value!(1), V.LHS)), OpSubA!(OpDiv!(OpMul!(T.RHS.LHS, T.LHS), OpAdd!(T.LHS, OpDiv!(Value!(1), V.LHS))), T.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (/> A (-> B X)) C) -> (/> A (-> (+ (/ C A) B) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpDivA!(T.LHS, OpSubA!(OpAdd!(OpDiv!(V, T.LHS), T.RHS.LHS), T.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (/> K (-> H X)) X) -> (/> (+ K 1) (-> (/ (* H K) (+ K 1)) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == UnDefined)
		 && is(T.RHS.RHS == V)
		) 
		alias OpDivA!(OpAdd!(T.LHS, Value!(1)), OpSubA!(OpDiv!(OpMul!(T.RHS.LHS, T.LHS), OpAdd!(T.LHS, Value!(1))), T.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (/> K (+> H X)) (/> F (+> E X))) -> (/> (+ K F) (+> (/ (+ (* H K) (* E F)) (+ K F)) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpAdd!(T.LHS, V.LHS), OpAddA!(OpDiv!(OpAdd!(OpMul!(T.RHS.LHS, T.LHS), OpMul!(V.RHS.LHS, V.LHS)), OpAdd!(T.LHS, V.LHS)), T.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (/> K (+> H X)) (*> F (+> E X))) -> (/> (+ K (/ 1 F)) (+> (/ (+ (* H K) (/ E F)) (+ K (/ 1 F))) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpAdd!(T.LHS, OpDiv!(Value!(1), V.LHS)), OpAddA!(OpDiv!(OpAdd!(OpMul!(T.RHS.LHS, T.LHS), OpDiv!(V.RHS.LHS, V.LHS)), OpAdd!(T.LHS, OpDiv!(Value!(1), V.LHS))), T.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (/> K (+> H X)) (/> E X)) -> (/> (+ K E) (+> (/ (* H K) (+ K E)) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS)
		) 
		alias OpDivA!(OpAdd!(T.LHS, V.LHS), OpAddA!(OpDiv!(OpMul!(T.RHS.LHS, T.LHS), OpAdd!(T.LHS, V.LHS)), T.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (/> K (+> H X)) (*> E X)) -> (/> (+ K (/ 1 E)) (+> (/ (* H K) (+ K (/ 1 E))) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS)
		) 
		alias OpDivA!(OpAdd!(T.LHS, OpDiv!(Value!(1), V.LHS)), OpAddA!(OpDiv!(OpMul!(T.RHS.LHS, T.LHS), OpAdd!(T.LHS, OpDiv!(Value!(1), V.LHS))), T.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (/> A (+> B X)) C) -> (/> A (+> (- B (/ C A)) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpDivA!(T.LHS, OpAddA!(OpSub!(T.RHS.LHS, OpDiv!(V, T.LHS)), T.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (/> K (+> H X)) X) -> (/> (+ K 1) (+> (/ (* H K) (+ K 1)) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == UnDefined)
		 && is(T.RHS.RHS == V)
		) 
		alias OpDivA!(OpAdd!(T.LHS, Value!(1)), OpAddA!(OpDiv!(OpMul!(T.RHS.LHS, T.LHS), OpAdd!(T.LHS, Value!(1))), T.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (*> K (-> H X)) (/> F (-> E X))) -> (/> (+ (/ 1 K) F) (-> (/ (+ (* H (/ 1 K)) (* E F)) (+ (/ 1 K) F)) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpAdd!(OpDiv!(Value!(1), T.LHS), V.LHS), OpSubA!(OpDiv!(OpAdd!(OpMul!(T.RHS.LHS, OpDiv!(Value!(1), T.LHS)), OpMul!(V.RHS.LHS, V.LHS)), OpAdd!(OpDiv!(Value!(1), T.LHS), V.LHS)), T.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (*> K (-> H X)) (*> F (-> E X))) -> (/> (+ (/ 1 K) (/ 1 F))
    (-> (/ (+ (/ E F) (/ H K)) (+ (/ 1 K) (/ 1 F))) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpAdd!(OpDiv!(Value!(1), T.LHS), OpDiv!(Value!(1), V.LHS)), OpSubA!(OpDiv!(OpAdd!(OpDiv!(V.RHS.LHS, V.LHS), OpDiv!(T.RHS.LHS, T.LHS)), OpAdd!(OpDiv!(Value!(1), T.LHS), OpDiv!(Value!(1), V.LHS))), T.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (*> K (-> H X)) (/> E X)) -> (/> (+ E (/ 1 K)) (-> (/ (/ H K) (+ E (/ 1 K))) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS)
		) 
		alias OpDivA!(OpAdd!(V.LHS, OpDiv!(Value!(1), T.LHS)), OpSubA!(OpDiv!(OpDiv!(T.RHS.LHS, T.LHS), OpAdd!(V.LHS, OpDiv!(Value!(1), T.LHS))), T.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (*> K (-> H X)) (*> E X)) -> (/> (+ (/ 1 K) (/ 1 E)) (-> (/ (/ H K) (+ (/ 1 K) (/ 1 E))) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS)
		) 
		alias OpDivA!(OpAdd!(OpDiv!(Value!(1), T.LHS), OpDiv!(Value!(1), V.LHS)), OpSubA!(OpDiv!(OpDiv!(T.RHS.LHS, T.LHS), OpAdd!(OpDiv!(Value!(1), T.LHS), OpDiv!(Value!(1), V.LHS))), T.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (*> K (+> H X)) (/> F (+> E X))) -> (/> (+ F (/ 1 K)) (+> (/ (+ (/ H K) (* E F)) (+ F (/ 1 K))) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpAdd!(V.LHS, OpDiv!(Value!(1), T.LHS)), OpAddA!(OpDiv!(OpAdd!(OpDiv!(T.RHS.LHS, T.LHS), OpMul!(V.RHS.LHS, V.LHS)), OpAdd!(V.LHS, OpDiv!(Value!(1), T.LHS))), T.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (*> K (+> H X)) (*> F (+> E X))) -> (/> (+ (/ 1 K) (/ 1 F))
    (+> (/ (+ (/ H K) (/ E F)) (+ (/ 1 K) (/ 1 F))) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpAdd!(OpDiv!(Value!(1), T.LHS), OpDiv!(Value!(1), V.LHS)), OpAddA!(OpDiv!(OpAdd!(OpDiv!(T.RHS.LHS, T.LHS), OpDiv!(V.RHS.LHS, V.LHS)), OpAdd!(OpDiv!(Value!(1), T.LHS), OpDiv!(Value!(1), V.LHS))), T.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (*> K (+> H X)) (/> E X)) -> (/> (+ (/ 1 K) E) (+> (/ (/ H K) (+ (/ 1 K) E)) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS)
		) 
		alias OpDivA!(OpAdd!(OpDiv!(Value!(1), T.LHS), V.LHS), OpAddA!(OpDiv!(OpDiv!(T.RHS.LHS, T.LHS), OpAdd!(OpDiv!(Value!(1), T.LHS), V.LHS)), T.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (*> K (+> H X)) (*> E X)) -> (/> (+ (/ 1 K) (/ 1 E)) (+> (/ (/ H K) (+ (/ 1 K) (/ 1 E))) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS)
		) 
		alias OpDivA!(OpAdd!(OpDiv!(Value!(1), T.LHS), OpDiv!(Value!(1), V.LHS)), OpAddA!(OpDiv!(OpDiv!(T.RHS.LHS, T.LHS), OpAdd!(OpDiv!(Value!(1), T.LHS), OpDiv!(Value!(1), V.LHS))), T.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (*> A (+> B X)) C) -> (*> A (+> (- B (* C A)) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpMulA!(T.LHS, OpAddA!(OpSub!(T.RHS.LHS, OpMul!(V, T.LHS)), T.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (*> K (+> H X)) X) -> (/> (+ (/ 1 K) 1) (+> (/ (/ H K) (+ (/ 1 K) 1)) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == UnDefined)
		 && is(T.RHS.RHS == V)
		) 
		alias OpDivA!(OpAdd!(OpDiv!(Value!(1), T.LHS), Value!(1)), OpAddA!(OpDiv!(OpDiv!(T.RHS.LHS, T.LHS), OpAdd!(OpDiv!(Value!(1), T.LHS), Value!(1))), T.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (/R> H X) (/R> E X)) -> (/R> (+ H E) X) +/
		is(T.Op == DivAR) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == DivAR) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS)
		) 
		alias OpDivAR!(OpAdd!(T.LHS, V.LHS), T.RHS) TypeOfAdd;
	else
	static if( /+ (+ (/> H X) (/> F (-> E X))) -> (/> (+ H F) (-> (/ (* E F) (+ H F)) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpAdd!(T.LHS, V.LHS), OpSubA!(OpDiv!(OpMul!(V.RHS.LHS, V.LHS), OpAdd!(T.LHS, V.LHS)), T.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (/> H X) (/> F (+> E X))) -> (/> (+ H F) (+> (/ (* E F) (+ H F)) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpAdd!(T.LHS, V.LHS), OpAddA!(OpDiv!(OpMul!(V.RHS.LHS, V.LHS), OpAdd!(T.LHS, V.LHS)), T.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (/> H X) (*> F (-> E X))) -> (/> (+ H (/ 1 F)) (-> (/ (/ E F) (+ H (/ 1 F))) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpAdd!(T.LHS, OpDiv!(Value!(1), V.LHS)), OpSubA!(OpDiv!(OpDiv!(V.RHS.LHS, V.LHS), OpAdd!(T.LHS, OpDiv!(Value!(1), V.LHS))), T.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (/> H X) (*> F (+> E X))) -> (/> (+ H (/ 1 F)) (+> (/ (/ E F) (+ H (/ 1 F))) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpAdd!(T.LHS, OpDiv!(Value!(1), V.LHS)), OpAddA!(OpDiv!(OpDiv!(V.RHS.LHS, V.LHS), OpAdd!(T.LHS, OpDiv!(Value!(1), V.LHS))), T.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (/> H X) (/> C X)) -> (/> (+ H C) X) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS)
		) 
		alias OpDivA!(OpAdd!(T.LHS, V.LHS), T.RHS) TypeOfAdd;
	else
	static if( /+ (+ (/> H X) (*> E X)) -> (/> (+ H (/ 1 E)) X) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS)
		) 
		alias OpDivA!(OpAdd!(T.LHS, OpDiv!(Value!(1), V.LHS)), T.RHS) TypeOfAdd;
	else
	static if( /+ (+ (/> A (-R> B X)) C) -> (/> A (-R> (+ B (/ C A)) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubAR) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpDivA!(T.LHS, OpSubAR!(OpAdd!(T.RHS.LHS, OpDiv!(V, T.LHS)), T.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (/> B X) A) -> (/> B (-> (/ A B) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpDivA!(T.LHS, OpSubA!(OpDiv!(V, T.LHS), T.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (/> H X) X) -> (/> (+ H 1) X) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == UnDefined)
		 && is(T.RHS == V)
		) 
		alias OpDivA!(OpAdd!(T.LHS, Value!(1)), T.RHS) TypeOfAdd;
	else
	static if( /+ (+ (*> H X) (/> F (-> E X))) -> (/> (+ (/ 1 H) F) (-> (/ (* E F) (+ (/ 1 H) F)) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpAdd!(OpDiv!(Value!(1), T.LHS), V.LHS), OpSubA!(OpDiv!(OpMul!(V.RHS.LHS, V.LHS), OpAdd!(OpDiv!(Value!(1), T.LHS), V.LHS)), T.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (*> H X) (/> F (+> E X))) -> (/> (+ (/ 1 H) F) (+> (/ (* E F) (+ (/ 1 H) F)) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpAdd!(OpDiv!(Value!(1), T.LHS), V.LHS), OpAddA!(OpDiv!(OpMul!(V.RHS.LHS, V.LHS), OpAdd!(OpDiv!(Value!(1), T.LHS), V.LHS)), T.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (*> H X) (*> F (-> E X))) -> (/> (+ (/ 1 H) (/ 1 F)) (-> (/ (/ E F) (+ (/ 1 H) (/ 1 F))) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpAdd!(OpDiv!(Value!(1), T.LHS), OpDiv!(Value!(1), V.LHS)), OpSubA!(OpDiv!(OpDiv!(V.RHS.LHS, V.LHS), OpAdd!(OpDiv!(Value!(1), T.LHS), OpDiv!(Value!(1), V.LHS))), T.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (*> H X) (*> F (+> E X))) -> (/> (+ (/ 1 H) (/ 1 F)) (+> (/ (/ E F) (+ (/ 1 H) (/ 1 F))) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpAdd!(OpDiv!(Value!(1), T.LHS), OpDiv!(Value!(1), V.LHS)), OpAddA!(OpDiv!(OpDiv!(V.RHS.LHS, V.LHS), OpAdd!(OpDiv!(Value!(1), T.LHS), OpDiv!(Value!(1), V.LHS))), T.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (*> H X) (/> E X)) -> (/> (+ (/ 1 H) E) X) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS)
		) 
		alias OpDivA!(OpAdd!(OpDiv!(Value!(1), T.LHS), V.LHS), T.RHS) TypeOfAdd;
	else
	static if( /+ (+ (*> H X) (*> E X)) -> (/> (+ (/ 1 H) (/ 1 E)) X) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS)
		) 
		alias OpDivA!(OpAdd!(OpDiv!(Value!(1), T.LHS), OpDiv!(Value!(1), V.LHS)), T.RHS) TypeOfAdd;
	else
	static if( /+ (+ (*> A (-R> B X)) C) -> (*> A (-R> (+ B (* C A)) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubAR) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpMulA!(T.LHS, OpSubAR!(OpAdd!(T.RHS.LHS, OpMul!(V, T.LHS)), T.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (*> A (-> B X)) C) -> (*> A (-> (+ (* C A) B) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpMulA!(T.LHS, OpSubA!(OpAdd!(OpMul!(V, T.LHS), T.RHS.LHS), T.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (*> B X) A) -> (*> B (-> (* A B) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpMulA!(T.LHS, OpSubA!(OpMul!(V, T.LHS), T.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (*> H X) X) -> (/> (+ (/ 1 H) 1) X) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == UnDefined)
		 && is(T.RHS == V)
		) 
		alias OpDivA!(OpAdd!(OpDiv!(Value!(1), T.LHS), Value!(1)), T.RHS) TypeOfAdd;
	else
	static if( /+ (+ (-> B X) A) -> (-> (+ A B) X) +/
		is(T.Op == SubA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpSubA!(OpAdd!(V, T.LHS), T.RHS) TypeOfAdd;
	else
	static if( /+ (+ (-> H X) X) -> (/> 2 (-> (/ H 2) X)) +/
		is(T.Op == SubA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == UnDefined)
		 && is(T.RHS == V)
		) 
		alias OpDivA!(Value!(2), OpSubA!(OpDiv!(T.LHS, Value!(2)), T.RHS)) TypeOfAdd;
	else
	static if( /+ (+ (+> B X) A) -> (+> (- B A) X) +/
		is(T.Op == AddA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpAddA!(OpSub!(T.LHS, V), T.RHS) TypeOfAdd;
	else
	static if( /+ (+ (+> H X) X) -> (/> 2 (+> (/ H 2) X)) +/
		is(T.Op == AddA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == UnDefined)
		 && is(T.RHS == V)
		) 
		alias OpDivA!(Value!(2), OpAddA!(OpDiv!(T.LHS, Value!(2)), T.RHS)) TypeOfAdd;
	else
	static if( /+ (+ C (/> A (-> B X))) -> (/> A (-> (+ (/ C A) B) X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivA!(V.LHS, OpSubA!(OpAdd!(OpDiv!(T, V.LHS), V.RHS.LHS), V.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ X (/> F (-> E X))) -> (/> (+ 1 F) (-> (/ (* E F) (+ 1 F)) X)) +/
		is(T.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T == V.RHS.RHS)
		) 
		alias OpDivA!(OpAdd!(Value!(1), V.LHS), OpSubA!(OpDiv!(OpMul!(V.RHS.LHS, V.LHS), OpAdd!(Value!(1), V.LHS)), T)) TypeOfAdd;
	else
	static if( /+ (+ C (/> A (+> B X))) -> (/> A (+> (- B (/ C A)) X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivA!(V.LHS, OpAddA!(OpSub!(V.RHS.LHS, OpDiv!(T, V.LHS)), V.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ X (/> F (+> E X))) -> (/> (+ 1 F) (+> (/ (* E F) (+ 1 F)) X)) +/
		is(T.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T == V.RHS.RHS)
		) 
		alias OpDivA!(OpAdd!(Value!(1), V.LHS), OpAddA!(OpDiv!(OpMul!(V.RHS.LHS, V.LHS), OpAdd!(Value!(1), V.LHS)), T)) TypeOfAdd;
	else
	static if( /+ (+ C (*> A (-> B X))) -> (*> A (-> (+ (* C A) B) X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpMulA!(V.LHS, OpSubA!(OpAdd!(OpMul!(T, V.LHS), V.RHS.LHS), V.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ X (*> F (-> E X))) -> (/> (+ 1 (/ 1 F)) (-> (/ (/ E F) (+ 1 (/ 1 F))) X)) +/
		is(T.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T == V.RHS.RHS)
		) 
		alias OpDivA!(OpAdd!(Value!(1), OpDiv!(Value!(1), V.LHS)), OpSubA!(OpDiv!(OpDiv!(V.RHS.LHS, V.LHS), OpAdd!(Value!(1), OpDiv!(Value!(1), V.LHS))), T)) TypeOfAdd;
	else
	static if( /+ (+ C (*> A (+> B X))) -> (*> A (+> (- B (* C A)) X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpMulA!(V.LHS, OpAddA!(OpSub!(V.RHS.LHS, OpMul!(T, V.LHS)), V.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ X (*> F (+> E X))) -> (/> (+ 1 (/ 1 F)) (+> (/ (/ E F) (+ 1 (/ 1 F))) X)) +/
		is(T.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T == V.RHS.RHS)
		) 
		alias OpDivA!(OpAdd!(Value!(1), OpDiv!(Value!(1), V.LHS)), OpAddA!(OpDiv!(OpDiv!(V.RHS.LHS, V.LHS), OpAdd!(Value!(1), OpDiv!(Value!(1), V.LHS))), T)) TypeOfAdd;
	else
	static if( /+ (+ C (/> A (-R> B X))) -> (/> A (-R> (+ B (/ C A)) X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubAR) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivA!(V.LHS, OpSubAR!(OpAdd!(V.RHS.LHS, OpDiv!(T, V.LHS)), V.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ A (/> B X)) -> (/> B (-> (/ A B) X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivA!(V.LHS, OpSubA!(OpDiv!(T, V.LHS), V.RHS)) TypeOfAdd;
	else
	static if( /+ (+ X (/> E X)) -> (/> (+ 1 E) X) +/
		is(T.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T == V.RHS)
		) 
		alias OpDivA!(OpAdd!(Value!(1), V.LHS), T) TypeOfAdd;
	else
	static if( /+ (+ C (*> A (-R> B X))) -> (*> A (-R> (+ B (* C A)) X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubAR) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpMulA!(V.LHS, OpSubAR!(OpAdd!(V.RHS.LHS, OpMul!(T, V.LHS)), V.RHS.RHS)) TypeOfAdd;
	else
	static if( /+ (+ A (*> B X)) -> (*> B (-> (* A B) X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpMulA!(V.LHS, OpSubA!(OpMul!(T, V.LHS), V.RHS)) TypeOfAdd;
	else
	static if( /+ (+ X (*> E X)) -> (/> (+ (/ 1 E) 1) X) +/
		is(T.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T == V.RHS)
		) 
		alias OpDivA!(OpAdd!(OpDiv!(Value!(1), V.LHS), Value!(1)), T) TypeOfAdd;
	else
	static if( /+ (+ A (-> B X)) -> (-> (+ A B) X) +/
		is(T.DefP == Defined) &&
		is(V.Op == SubA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpSubA!(OpAdd!(T, V.LHS), V.RHS) TypeOfAdd;
	else
	static if( /+ (+ X (-> E X)) -> (/> 2 (-> (/ E 2) X)) +/
		is(T.DefP == UnDefined) &&
		is(V.Op == SubA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T == V.RHS)
		) 
		alias OpDivA!(Value!(2), OpSubA!(OpDiv!(V.LHS, Value!(2)), T)) TypeOfAdd;
	else
	static if( /+ (+ A (+> B X)) -> (+> (- B A) X) +/
		is(T.DefP == Defined) &&
		is(V.Op == AddA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpAddA!(OpSub!(V.LHS, T), V.RHS) TypeOfAdd;
	else
	static if( /+ (+ X (+> E X)) -> (/> 2 (+> (/ E 2) X)) +/
		is(T.DefP == UnDefined) &&
		is(V.Op == AddA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T == V.RHS)
		) 
		alias OpDivA!(Value!(2), OpAddA!(OpDiv!(V.LHS, Value!(2)), T)) TypeOfAdd;
	else
	static if( /+ (+ (-R> B X) A) -> (-R> (+ A B) X) +/
		is(T.Op == SubAR) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpSubAR!(OpAdd!(V, T.LHS), T.RHS) TypeOfAdd;
	else
	static if( /+ (+ A (-R> B X)) -> (-R> (+ A B) X) +/
		is(T.DefP == Defined) &&
		is(V.Op == SubAR) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpSubAR!(OpAdd!(T, V.LHS), V.RHS) TypeOfAdd;
	else
	static if( /+ (+ A B) -> (+ A B) +/
		is(T.DefP == Defined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpAdd!(T, V) TypeOfAdd;
	else
	static if( /+ (+ A X) -> (-> A X) +/
		is(T.DefP == Defined) &&
		is(V.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpSubA!(T, V) TypeOfAdd;
	else
	static if( /+ (+ X A) -> (-> A X) +/
		is(T.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpSubA!(V, T) TypeOfAdd;
	else
	static if( /+ (+ X X) -> (/> 2 X) +/
		is(T.DefP == UnDefined) &&
		is(V.DefP == UnDefined)
		 && is(T == V)
		) 
		alias OpDivA!(Value!(2), T) TypeOfAdd;
	else
		{}
	static if(!is(TypeOfAdd))
		static assert(false, `unusable types used for Add: (+ ` ~ T.LispOf ~ ` `~ V.LispOf ~ `)` );
}
template TypeOfSub(T, V)
{
	static if( /+ (- (/> K (-> H X)) (/> F (-> E X))) -> (/> (- K F) (-> (/ (- (* H K) (* E F)) (- K F)) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpSub!(T.LHS, V.LHS), OpSubA!(OpDiv!(OpSub!(OpMul!(T.RHS.LHS, T.LHS), OpMul!(V.RHS.LHS, V.LHS)), OpSub!(T.LHS, V.LHS)), T.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- (/> K (-> H X)) (*> F (-> E X))) -> (/> (- K (/ 1 F)) (-> (/ (- (* H K) (/ E F)) (- K (/ 1 F))) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpSub!(T.LHS, OpDiv!(Value!(1), V.LHS)), OpSubA!(OpDiv!(OpSub!(OpMul!(T.RHS.LHS, T.LHS), OpDiv!(V.RHS.LHS, V.LHS)), OpSub!(T.LHS, OpDiv!(Value!(1), V.LHS))), T.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- (/> K (-> H X)) (/> E X)) -> (/> (- K E) (-> (/ (* H K) (- K E)) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS)
		) 
		alias OpDivA!(OpSub!(T.LHS, V.LHS), OpSubA!(OpDiv!(OpMul!(T.RHS.LHS, T.LHS), OpSub!(T.LHS, V.LHS)), T.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- (/> K (-> H X)) (*> E X)) -> (/> (- K (/ 1 E)) (-> (/ (* H K) (- K (/ 1 E))) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS)
		) 
		alias OpDivA!(OpSub!(T.LHS, OpDiv!(Value!(1), V.LHS)), OpSubA!(OpDiv!(OpMul!(T.RHS.LHS, T.LHS), OpSub!(T.LHS, OpDiv!(Value!(1), V.LHS))), T.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- (/> K (+> H X)) (/> F (+> E X))) -> (/> (- K F) (-> (/ (- (* E F) (* H K)) (- K F)) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpSub!(T.LHS, V.LHS), OpSubA!(OpDiv!(OpSub!(OpMul!(V.RHS.LHS, V.LHS), OpMul!(T.RHS.LHS, T.LHS)), OpSub!(T.LHS, V.LHS)), T.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- (/> K (+> H X)) (*> F (+> E X))) -> (/> (- K (/ 1 F)) (-> (/ (- (/ E F) (* H K)) (- K (/ 1 F))) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpSub!(T.LHS, OpDiv!(Value!(1), V.LHS)), OpSubA!(OpDiv!(OpSub!(OpDiv!(V.RHS.LHS, V.LHS), OpMul!(T.RHS.LHS, T.LHS)), OpSub!(T.LHS, OpDiv!(Value!(1), V.LHS))), T.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- (/> K (+> H X)) (/> E X)) -> (/> (- K E) (+> (/ (* H K) (- K E)) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS)
		) 
		alias OpDivA!(OpSub!(T.LHS, V.LHS), OpAddA!(OpDiv!(OpMul!(T.RHS.LHS, T.LHS), OpSub!(T.LHS, V.LHS)), T.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- (/> K (+> H X)) (*> E X)) -> (/> (- K (/ 1 E)) (+> (/ (* H K) (- K (/ 1 E))) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS)
		) 
		alias OpDivA!(OpSub!(T.LHS, OpDiv!(Value!(1), V.LHS)), OpAddA!(OpDiv!(OpMul!(T.RHS.LHS, T.LHS), OpSub!(T.LHS, OpDiv!(Value!(1), V.LHS))), T.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- (*> K (-> H X)) (/> F (-> E X))) -> (/> (- (/ 1 K) F) (-> (/ (- (/ H K) (* E F)) (- (/ 1 K) F)) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpSub!(OpDiv!(Value!(1), T.LHS), V.LHS), OpSubA!(OpDiv!(OpSub!(OpDiv!(T.RHS.LHS, T.LHS), OpMul!(V.RHS.LHS, V.LHS)), OpSub!(OpDiv!(Value!(1), T.LHS), V.LHS)), T.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- (*> K (-> H X)) (*> F (-> E X))) -> (/> (- (/ 1 K) (/ 1 F))
    (-> (/ (- (/ H K) (/ E F)) (- (/ 1 K) (/ 1 F))) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpSub!(OpDiv!(Value!(1), T.LHS), OpDiv!(Value!(1), V.LHS)), OpSubA!(OpDiv!(OpSub!(OpDiv!(T.RHS.LHS, T.LHS), OpDiv!(V.RHS.LHS, V.LHS)), OpSub!(OpDiv!(Value!(1), T.LHS), OpDiv!(Value!(1), V.LHS))), T.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- (*> K (-> H X)) (/> E X)) -> (/> (- (/ 1 K) E) (-> (/ (/ H K) (- (/ 1 K) E)) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS)
		) 
		alias OpDivA!(OpSub!(OpDiv!(Value!(1), T.LHS), V.LHS), OpSubA!(OpDiv!(OpDiv!(T.RHS.LHS, T.LHS), OpSub!(OpDiv!(Value!(1), T.LHS), V.LHS)), T.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- (*> K (-> H X)) (*> E X)) -> (/> (- (/ 1 K) (/ 1 E)) (-> (/ (/ H K) (- (/ 1 K) (/ 1 E))) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS)
		) 
		alias OpDivA!(OpSub!(OpDiv!(Value!(1), T.LHS), OpDiv!(Value!(1), V.LHS)), OpSubA!(OpDiv!(OpDiv!(T.RHS.LHS, T.LHS), OpSub!(OpDiv!(Value!(1), T.LHS), OpDiv!(Value!(1), V.LHS))), T.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- (*> K (+> H X)) (/> F (+> E X))) -> (/> (- (/ 1 K) F) (-> (/ (- (* E F) (/ H K)) (- (/ 1 K) F)) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpSub!(OpDiv!(Value!(1), T.LHS), V.LHS), OpSubA!(OpDiv!(OpSub!(OpMul!(V.RHS.LHS, V.LHS), OpDiv!(T.RHS.LHS, T.LHS)), OpSub!(OpDiv!(Value!(1), T.LHS), V.LHS)), T.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- (*> K (+> H X)) (*> F (+> E X))) -> (/> (- (/ 1 K) (/ 1 F))
    (-> (/ (- (/ E F) (/ H K)) (- (/ 1 K) (/ 1 F))) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpSub!(OpDiv!(Value!(1), T.LHS), OpDiv!(Value!(1), V.LHS)), OpSubA!(OpDiv!(OpSub!(OpDiv!(V.RHS.LHS, V.LHS), OpDiv!(T.RHS.LHS, T.LHS)), OpSub!(OpDiv!(Value!(1), T.LHS), OpDiv!(Value!(1), V.LHS))), T.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- (*> K (+> H X)) (/> E X)) -> (/> (- (/ 1 K) E) (+> (/ (/ H K) (- (/ 1 K) E)) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS)
		) 
		alias OpDivA!(OpSub!(OpDiv!(Value!(1), T.LHS), V.LHS), OpAddA!(OpDiv!(OpDiv!(T.RHS.LHS, T.LHS), OpSub!(OpDiv!(Value!(1), T.LHS), V.LHS)), T.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- (*> K (+> H X)) (*> E X)) -> (/> (- (/ 1 K) (/ 1 E)) (+> (/ (/ H K) (- (/ 1 K) (/ 1 E))) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS.RHS == V.RHS)
		) 
		alias OpDivA!(OpSub!(OpDiv!(Value!(1), T.LHS), OpDiv!(Value!(1), V.LHS)), OpAddA!(OpDiv!(OpDiv!(T.RHS.LHS, T.LHS), OpSub!(OpDiv!(Value!(1), T.LHS), OpDiv!(Value!(1), V.LHS))), T.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- (/R> H X) (/R> E X)) -> (/R> (- H E) X) +/
		is(T.Op == DivAR) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == DivAR) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS)
		) 
		alias OpDivAR!(OpSub!(T.LHS, V.LHS), T.RHS) TypeOfSub;
	else
	static if( /+ (- (-R> B X) A) -> (-R> (- B A) X) +/
		is(T.Op == SubAR) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpSubAR!(OpSub!(T.LHS, V), T.RHS) TypeOfSub;
	else
	static if( /+ (- (-R> H X) X) -> (/> 2 (-R> (/ H 2) X)) +/
		is(T.Op == SubAR) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == UnDefined)
		 && is(T.RHS == V)
		) 
		alias OpDivA!(Value!(2), OpSubAR!(OpDiv!(T.LHS, Value!(2)), T.RHS)) TypeOfSub;
	else
	static if( /+ (- (/> H X) (/> F (-> E X))) -> (/> (- H F) (+> (/ (* E F) (- H F)) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpSub!(T.LHS, V.LHS), OpAddA!(OpDiv!(OpMul!(V.RHS.LHS, V.LHS), OpSub!(T.LHS, V.LHS)), T.RHS)) TypeOfSub;
	else
	static if( /+ (- (/> H X) (/> F (+> E X))) -> (/> (- H F) (-> (/ (* E F) (- H F)) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpSub!(T.LHS, V.LHS), OpSubA!(OpDiv!(OpMul!(V.RHS.LHS, V.LHS), OpSub!(T.LHS, V.LHS)), T.RHS)) TypeOfSub;
	else
	static if( /+ (- (/> H X) (*> F (-> E X))) -> (/> (- H (/ 1 F)) (+> (/ (/ E F) (- H (/ 1 F))) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpSub!(T.LHS, OpDiv!(Value!(1), V.LHS)), OpAddA!(OpDiv!(OpDiv!(V.RHS.LHS, V.LHS), OpSub!(T.LHS, OpDiv!(Value!(1), V.LHS))), T.RHS)) TypeOfSub;
	else
	static if( /+ (- (/> H X) (*> F (+> E X))) -> (/> (- H (/ 1 F)) (-> (/ (/ E F) (- H (/ 1 F))) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpSub!(T.LHS, OpDiv!(Value!(1), V.LHS)), OpSubA!(OpDiv!(OpDiv!(V.RHS.LHS, V.LHS), OpSub!(T.LHS, OpDiv!(Value!(1), V.LHS))), T.RHS)) TypeOfSub;
	else
	static if( /+ (- (/> H X) (/> E X)) -> (/> (- H E) X) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS)
		) 
		alias OpDivA!(OpSub!(T.LHS, V.LHS), T.RHS) TypeOfSub;
	else
	static if( /+ (- (/> H X) (*> E X)) -> (/> (- H (/ 1 E)) X) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS)
		) 
		alias OpDivA!(OpSub!(T.LHS, OpDiv!(Value!(1), V.LHS)), T.RHS) TypeOfSub;
	else
	static if( /+ (- (/> A (-R> B X)) C) -> (/> A (-R> (- B (/ C A)) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubAR) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpDivA!(T.LHS, OpSubAR!(OpSub!(T.RHS.LHS, OpDiv!(V, T.LHS)), T.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- (/> A (+> B X)) C) -> (/> A (+> (+ (/ C A) B) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpDivA!(T.LHS, OpAddA!(OpAdd!(OpDiv!(V, T.LHS), T.RHS.LHS), T.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- (/> A (-> B X)) C) -> (/> A (+> (- (/ C A) B) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpDivA!(T.LHS, OpAddA!(OpSub!(OpDiv!(V, T.LHS), T.RHS.LHS), T.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- (/> B X) A) -> (/> B (+> (/ A B) X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpDivA!(T.LHS, OpAddA!(OpDiv!(V, T.LHS), T.RHS)) TypeOfSub;
	else
	static if( /+ (- (/> H X) X) -> (/> (- H 1) X) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == UnDefined)
		 && is(T.RHS == V)
		) 
		alias OpDivA!(OpSub!(T.LHS, Value!(1)), T.RHS) TypeOfSub;
	else
	static if( /+ (- (*> H X) (/> F (-> E X))) -> (/> (- (/ 1 H) F) (+> (/ (* E F) (- (/ 1 H) F)) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpSub!(OpDiv!(Value!(1), T.LHS), V.LHS), OpAddA!(OpDiv!(OpMul!(V.RHS.LHS, V.LHS), OpSub!(OpDiv!(Value!(1), T.LHS), V.LHS)), T.RHS)) TypeOfSub;
	else
	static if( /+ (- (*> H X) (/> F (+> E X))) -> (/> (- (/ 1 H) F) (-> (/ (* E F) (- (/ 1 H) F)) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpSub!(OpDiv!(Value!(1), T.LHS), V.LHS), OpSubA!(OpDiv!(OpMul!(V.RHS.LHS, V.LHS), OpSub!(OpDiv!(Value!(1), T.LHS), V.LHS)), T.RHS)) TypeOfSub;
	else
	static if( /+ (- (*> H X) (*> A (-> B X))) -> (+> (/ B A) (/> (- (/ 1 H) (/ 1 A)) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS.RHS)
		) 
		alias OpAddA!(OpDiv!(V.RHS.LHS, V.LHS), OpDivA!(OpSub!(OpDiv!(Value!(1), T.LHS), OpDiv!(Value!(1), V.LHS)), T.RHS)) TypeOfSub;
	else
	static if( /+ (- (*> H X) (*> F (+> E X))) -> (/> (- (/ 1 H) (/ 1 F)) (+> (/ (/ E F) (- (/ 1 H) (/ 1 F))) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS.RHS)
		) 
		alias OpDivA!(OpSub!(OpDiv!(Value!(1), T.LHS), OpDiv!(Value!(1), V.LHS)), OpAddA!(OpDiv!(OpDiv!(V.RHS.LHS, V.LHS), OpSub!(OpDiv!(Value!(1), T.LHS), OpDiv!(Value!(1), V.LHS))), T.RHS)) TypeOfSub;
	else
	static if( /+ (- (*> H X) (/> E X)) -> (/> (- (/ 1 H) E) X) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS)
		) 
		alias OpDivA!(OpSub!(OpDiv!(Value!(1), T.LHS), V.LHS), T.RHS) TypeOfSub;
	else
	static if( /+ (- (*> H X) (*> E X)) -> (/> (- (/ 1 H) (/ 1 E)) X) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T.RHS == V.RHS)
		) 
		alias OpDivA!(OpSub!(OpDiv!(Value!(1), T.LHS), OpDiv!(Value!(1), V.LHS)), T.RHS) TypeOfSub;
	else
	static if( /+ (- (*> A (-R> B X)) C) -> (*> A (-R> (- B (* C A)) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubAR) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpMulA!(T.LHS, OpSubAR!(OpSub!(T.RHS.LHS, OpMul!(V, T.LHS)), T.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- (*> A (+> B X)) C) -> (*> A (+> (+ (* C A) B) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpMulA!(T.LHS, OpAddA!(OpAdd!(OpMul!(V, T.LHS), T.RHS.LHS), T.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- (*> A (-> B X)) C) -> (*> A (+> (- (* C A) B) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpMulA!(T.LHS, OpAddA!(OpSub!(OpMul!(V, T.LHS), T.RHS.LHS), T.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- (*> B X) A) -> (*> B (+> (* A B) X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpMulA!(T.LHS, OpAddA!(OpMul!(V, T.LHS), T.RHS)) TypeOfSub;
	else
	static if( /+ (- (*> H X) X) -> (/> (- (/ 1 H) 1) X) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == UnDefined)
		 && is(T.RHS == V)
		) 
		alias OpDivA!(OpSub!(OpDiv!(Value!(1), T.LHS), Value!(1)), T.RHS) TypeOfSub;
	else
	static if( /+ (- C (/> A (-R> B X))) -> (/> A (+> (- B (/ C A)) X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubAR) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivA!(V.LHS, OpAddA!(OpSub!(V.RHS.LHS, OpDiv!(T, V.LHS)), V.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- X (/> F (-R> E X))) -> (/> (+ 1 F) (-> (/ (* E F) (+ 1 F)) X)) +/
		is(T.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubAR) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T == V.RHS.RHS)
		) 
		alias OpDivA!(OpAdd!(Value!(1), V.LHS), OpSubA!(OpDiv!(OpMul!(V.RHS.LHS, V.LHS), OpAdd!(Value!(1), V.LHS)), T)) TypeOfSub;
	else
	static if( /+ (- C (/> A (-> B X))) -> (/> A (-R> (- (/ C A) B) X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivA!(V.LHS, OpSubAR!(OpSub!(OpDiv!(T, V.LHS), V.RHS.LHS), V.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- X (/> F (-> E X))) -> (/> (- 1 F) (+> (/ (* E F) (- 1 F)) X)) +/
		is(T.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T == V.RHS.RHS)
		) 
		alias OpDivA!(OpSub!(Value!(1), V.LHS), OpAddA!(OpDiv!(OpMul!(V.RHS.LHS, V.LHS), OpSub!(Value!(1), V.LHS)), T)) TypeOfSub;
	else
	static if( /+ (- C (/> A (+> B X))) -> (/> A (-R> (+ (/ C A) B) X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivA!(V.LHS, OpSubAR!(OpAdd!(OpDiv!(T, V.LHS), V.RHS.LHS), V.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- X (/> F (+> E X))) -> (/> (- 1 F) (-> (/ (* E F) (- 1 F)) X)) +/
		is(T.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T == V.RHS.RHS)
		) 
		alias OpDivA!(OpSub!(Value!(1), V.LHS), OpSubA!(OpDiv!(OpMul!(V.RHS.LHS, V.LHS), OpSub!(Value!(1), V.LHS)), T)) TypeOfSub;
	else
	static if( /+ (- C (*> A (-R> B X))) -> (*> A (+> (- B (* C A)) X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubAR) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpMulA!(V.LHS, OpAddA!(OpSub!(V.RHS.LHS, OpMul!(T, V.LHS)), V.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- X (*> F (-R> E X))) -> (/> (+ 1 (/ 1 F)) (+> (/ (/ E F) (+ 1 (/ 1 F))) X)) +/
		is(T.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubAR) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T == V.RHS.RHS)
		) 
		alias OpDivA!(OpAdd!(Value!(1), OpDiv!(Value!(1), V.LHS)), OpAddA!(OpDiv!(OpDiv!(V.RHS.LHS, V.LHS), OpAdd!(Value!(1), OpDiv!(Value!(1), V.LHS))), T)) TypeOfSub;
	else
	static if( /+ (- C (*> A (-> B X))) -> (*> A (-R> (- (* A C) B) X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpMulA!(V.LHS, OpSubAR!(OpSub!(OpMul!(V.LHS, T), V.RHS.LHS), V.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- X (*> F (-> E X))) -> (/> (- 1 (/ 1 F)) (+> (/ (/ E F) (- 1 (/ 1 F))) X)) +/
		is(T.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T == V.RHS.RHS)
		) 
		alias OpDivA!(OpSub!(Value!(1), OpDiv!(Value!(1), V.LHS)), OpAddA!(OpDiv!(OpDiv!(V.RHS.LHS, V.LHS), OpSub!(Value!(1), OpDiv!(Value!(1), V.LHS))), T)) TypeOfSub;
	else
	static if( /+ (- C (*> A (+> B X))) -> (*> A (-R> (+ (* C A) B) X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpMulA!(V.LHS, OpSubAR!(OpAdd!(OpMul!(T, V.LHS), V.RHS.LHS), V.RHS.RHS)) TypeOfSub;
	else
	static if( /+ (- X (*> F (+> E X))) -> (/> (- 1 (/ 1 F)) (-> (/ (/ E F) (- 1 (/ 1 F))) X)) +/
		is(T.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 && is(T == V.RHS.RHS)
		) 
		alias OpDivA!(OpSub!(Value!(1), OpDiv!(Value!(1), V.LHS)), OpSubA!(OpDiv!(OpDiv!(V.RHS.LHS, V.LHS), OpSub!(Value!(1), OpDiv!(Value!(1), V.LHS))), T)) TypeOfSub;
	else
	static if( /+ (- A (-R> B X)) -> (+> (- B A) X) +/
		is(T.DefP == Defined) &&
		is(V.Op == SubAR) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpAddA!(OpSub!(V.LHS, T), V.RHS) TypeOfSub;
	else
	static if( /+ (- X (-R> E X)) -> (/> 2 (+> (/ E 2) X)) +/
		is(T.DefP == UnDefined) &&
		is(V.Op == SubAR) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T == V.RHS)
		) 
		alias OpDivA!(Value!(2), OpAddA!(OpDiv!(V.LHS, Value!(2)), T)) TypeOfSub;
	else
	static if( /+ (- A (/> B X)) -> (/> B (-R> (/ A B) X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivA!(V.LHS, OpSubAR!(OpDiv!(T, V.LHS), V.RHS)) TypeOfSub;
	else
	static if( /+ (- X (/> E X)) -> (/> (- 1 E) X) +/
		is(T.DefP == UnDefined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T == V.RHS)
		) 
		alias OpDivA!(OpSub!(Value!(1), V.LHS), T) TypeOfSub;
	else
	static if( /+ (- A (*> B X)) -> (*> B (-R> (* A B) X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpMulA!(V.LHS, OpSubAR!(OpMul!(T, V.LHS), V.RHS)) TypeOfSub;
	else
	static if( /+ (- X (*> E X)) -> (/> (- 1 (/ 1 E)) X) +/
		is(T.DefP == UnDefined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T == V.RHS)
		) 
		alias OpDivA!(OpSub!(Value!(1), OpDiv!(Value!(1), V.LHS)), T) TypeOfSub;
	else
	static if( /+ (- A (-> B X)) -> (-R> (- A B) X) +/
		is(T.DefP == Defined) &&
		is(V.Op == SubA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpSubAR!(OpSub!(T, V.LHS), V.RHS) TypeOfSub;
	else
	static if( /+ (- X (-> E X)) -> (- 0 E) +/
		is(T.DefP == UnDefined) &&
		is(V.Op == SubA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T == V.RHS)
		) 
		alias OpSub!(Value!(0), V.LHS) TypeOfSub;
	else
	static if( /+ (- A (+> B X)) -> (-R> (+ A B) X) +/
		is(T.DefP == Defined) &&
		is(V.Op == AddA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpSubAR!(OpAdd!(T, V.LHS), V.RHS) TypeOfSub;
	else
	static if( /+ (- X (+> E X)) -> E +/
		is(T.DefP == UnDefined) &&
		is(V.Op == AddA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 && is(T == V.RHS)
		) 
		alias V.LHS TypeOfSub;
	else
	static if( /+ (- (+> B X) A) -> (+> (+ A B) X) +/
		is(T.Op == AddA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpAddA!(OpAdd!(V, T.LHS), T.RHS) TypeOfSub;
	else
	static if( /+ (- (-> B X) A) -> (+> (- A B) X) +/
		is(T.Op == SubA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpAddA!(OpSub!(V, T.LHS), T.RHS) TypeOfSub;
	else
	static if( /+ (- A B) -> (- A B) +/
		is(T.DefP == Defined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpSub!(T, V) TypeOfSub;
	else
	static if( /+ (- A X) -> (-R> A X) +/
		is(T.DefP == Defined) &&
		is(V.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpSubAR!(T, V) TypeOfSub;
	else
	static if( /+ (- X A) -> (+> A X) +/
		is(T.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpAddA!(V, T) TypeOfSub;
	else
	static if( /+ (- X X) -> 0 +/
		is(T.DefP == UnDefined) &&
		is(V.DefP == UnDefined)
		 && is(T == V)
		) 
		alias Value!(0) TypeOfSub;
	else
		{}
	static if(!is(TypeOfSub))
		static assert(false, `unusable types used for Sub: (- ` ~ T.LispOf ~ ` `~ V.LispOf ~ `)` );
}
template TypeOfMul(T, V)
{
	static if( /+ (* (/> A (-R> B X)) C) -> (/> (* A C) (-R> B X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubAR) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpDivA!(OpMul!(T.LHS, V), OpSubAR!(T.RHS.LHS, T.RHS.RHS)) TypeOfMul;
	else
	static if( /+ (* C (/> A (-R> B X))) -> (/> (* A C) (-R> B X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubAR) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivA!(OpMul!(V.LHS, T), OpSubAR!(V.RHS.LHS, V.RHS.RHS)) TypeOfMul;
	else
	static if( /+ (* (*> A (-R> B X)) C) -> (*> (/ A C) (-R> B X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubAR) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpMulA!(OpDiv!(T.LHS, V), OpSubAR!(T.RHS.LHS, T.RHS.RHS)) TypeOfMul;
	else
	static if( /+ (* C (*> A (-R> B X))) -> (*> (/ A C) (-R> B X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubAR) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpMulA!(OpDiv!(V.LHS, T), OpSubAR!(V.RHS.LHS, V.RHS.RHS)) TypeOfMul;
	else
	static if( /+ (* (/R> A (+> B X)) C) -> (/R> (* A C) (+> B X)) +/
		is(T.Op == DivAR) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpDivAR!(OpMul!(T.LHS, V), OpAddA!(T.RHS.LHS, T.RHS.RHS)) TypeOfMul;
	else
	static if( /+ (* C (/R> A (+> B X))) -> (/R> (* A C) (+> B X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == DivAR) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivAR!(OpMul!(V.LHS, T), OpAddA!(V.RHS.LHS, V.RHS.RHS)) TypeOfMul;
	else
	static if( /+ (* (/> A (+> B X)) C) -> (/> (* C A) (+> B X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpDivA!(OpMul!(V, T.LHS), OpAddA!(T.RHS.LHS, T.RHS.RHS)) TypeOfMul;
	else
	static if( /+ (* C (/> A (+> B X))) -> (/> (* C A) (+> B X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivA!(OpMul!(T, V.LHS), OpAddA!(V.RHS.LHS, V.RHS.RHS)) TypeOfMul;
	else
	static if( /+ (* (*> A (+> B X)) C) -> (*> (/ A C) (+> B X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpMulA!(OpDiv!(T.LHS, V), OpAddA!(T.RHS.LHS, T.RHS.RHS)) TypeOfMul;
	else
	static if( /+ (* C (*> A (+> B X))) -> (*> (/ A C) (+> B X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpMulA!(OpDiv!(V.LHS, T), OpAddA!(V.RHS.LHS, V.RHS.RHS)) TypeOfMul;
	else
	static if( /+ (* (/R> A (-> B X)) C) -> (/R> (* A C) (-> B X)) +/
		is(T.Op == DivAR) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpDivAR!(OpMul!(T.LHS, V), OpSubA!(T.RHS.LHS, T.RHS.RHS)) TypeOfMul;
	else
	static if( /+ (* C (/R> A (-> B X))) -> (/R> (* A C) (-> B X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == DivAR) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivAR!(OpMul!(V.LHS, T), OpSubA!(V.RHS.LHS, V.RHS.RHS)) TypeOfMul;
	else
	static if( /+ (* (/> A (-> B X)) C) -> (/> (* C A) (-> B X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpDivA!(OpMul!(V, T.LHS), OpSubA!(T.RHS.LHS, T.RHS.RHS)) TypeOfMul;
	else
	static if( /+ (* C (/> A (-> B X))) -> (/> (* C A) (-> B X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivA!(OpMul!(T, V.LHS), OpSubA!(V.RHS.LHS, V.RHS.RHS)) TypeOfMul;
	else
	static if( /+ (* (*> A (-> B X)) C) -> (*> (/ A C) (-> B X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpMulA!(OpDiv!(T.LHS, V), OpSubA!(T.RHS.LHS, T.RHS.RHS)) TypeOfMul;
	else
	static if( /+ (* C (*> A (-> B X))) -> (*> (/ A C) (-> B X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpMulA!(OpDiv!(V.LHS, T), OpSubA!(V.RHS.LHS, V.RHS.RHS)) TypeOfMul;
	else
	static if( /+ (* (/R> B X) A) -> (/R> (* A B) X) +/
		is(T.Op == DivAR) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpDivAR!(OpMul!(V, T.LHS), T.RHS) TypeOfMul;
	else
	static if( /+ (* A (/R> B X)) -> (/R> (* A B) X) +/
		is(T.DefP == Defined) &&
		is(V.Op == DivAR) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivAR!(OpMul!(T, V.LHS), V.RHS) TypeOfMul;
	else
	static if( /+ (* (*> B X) A) -> (*> (/ B A) X) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpMulA!(OpDiv!(T.LHS, V), T.RHS) TypeOfMul;
	else
	static if( /+ (* A (*> B X)) -> (*> (/ B A) X) +/
		is(T.DefP == Defined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpMulA!(OpDiv!(V.LHS, T), V.RHS) TypeOfMul;
	else
	static if( /+ (* (/> B X) A) -> (/> (* A B) X) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpDivA!(OpMul!(V, T.LHS), T.RHS) TypeOfMul;
	else
	static if( /+ (* A (/> B X)) -> (/> (* A B) X) +/
		is(T.DefP == Defined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivA!(OpMul!(T, V.LHS), V.RHS) TypeOfMul;
	else
	static if( /+ (* (+> B X) A) -> (/> A (+> B X)) +/
		is(T.Op == AddA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpDivA!(V, OpAddA!(T.LHS, T.RHS)) TypeOfMul;
	else
	static if( /+ (* A (+> B X)) -> (/> A (+> B X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == AddA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivA!(T, OpAddA!(V.LHS, V.RHS)) TypeOfMul;
	else
	static if( /+ (* (-> B X) A) -> (/> A (-> B X)) +/
		is(T.Op == SubA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpDivA!(V, OpSubA!(T.LHS, T.RHS)) TypeOfMul;
	else
	static if( /+ (* A (-> B X)) -> (/> A (-> B X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == SubA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivA!(T, OpSubA!(V.LHS, V.RHS)) TypeOfMul;
	else
	static if( /+ (* A B) -> (* A B) +/
		is(T.DefP == Defined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpMul!(T, V) TypeOfMul;
	else
	static if( /+ (* A X) -> (/> A X) +/
		is(T.DefP == Defined) &&
		is(V.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivA!(T, V) TypeOfMul;
	else
	static if( /+ (* X A) -> (/> A X) +/
		is(T.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpDivA!(V, T) TypeOfMul;
	else
		{}
	static if(!is(TypeOfMul))
		static assert(false, `unusable types used for Mul: (* ` ~ T.LispOf ~ ` `~ V.LispOf ~ `)` );
}
template TypeOfDiv(T, V)
{
	static if( /+ (/ (-R> H X) X) -> (*> H (+> H X)) +/
		is(T.Op == SubAR) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == UnDefined)
		 && is(T.RHS == V)
		) 
		alias OpMulA!(T.LHS, OpAddA!(T.LHS, T.RHS)) TypeOfDiv;
	else
	static if( /+ (/ (-> B X) A) -> (*> A (-> B X)) +/
		is(T.Op == SubA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpMulA!(V, OpSubA!(T.LHS, T.RHS)) TypeOfDiv;
	else
	static if( /+ (/ (-> H X) X) -> (-> 1 (/R> H X)) +/
		is(T.Op == SubA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == UnDefined)
		 && is(T.RHS == V)
		) 
		alias OpSubA!(Value!(1), OpDivAR!(T.LHS, T.RHS)) TypeOfDiv;
	else
	static if( /+ (/ (+> B X) A) -> (*> A (+> B X)) +/
		is(T.Op == AddA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpMulA!(V, OpAddA!(T.LHS, T.RHS)) TypeOfDiv;
	else
	static if( /+ (/ (+> H X) X) -> (-R> 1 (/R> H X)) +/
		is(T.Op == AddA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == UnDefined)
		 && is(T.RHS == V)
		) 
		alias OpSubAR!(Value!(1), OpDivAR!(T.LHS, T.RHS)) TypeOfDiv;
	else
	static if( /+ (/ (/> A (-R> B X)) C) -> (*> (/ C A) (-R> B X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubAR) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpMulA!(OpDiv!(V, T.LHS), OpSubAR!(T.RHS.LHS, T.RHS.RHS)) TypeOfDiv;
	else
	static if( /+ (/ C (/> A (-R> B X))) -> (/R> (/ C A) (-R> B X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubAR) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivAR!(OpDiv!(T, V.LHS), OpSubAR!(V.RHS.LHS, V.RHS.RHS)) TypeOfDiv;
	else
	static if( /+ (/ (*> A (-R> B X)) C) -> (*> (* A C) (-R> B X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubAR) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpMulA!(OpMul!(T.LHS, V), OpSubAR!(T.RHS.LHS, T.RHS.RHS)) TypeOfDiv;
	else
	static if( /+ (/ C (*> A (-R> B X))) -> (/R> (* A C) (-R> B X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubAR) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivAR!(OpMul!(V.LHS, T), OpSubAR!(V.RHS.LHS, V.RHS.RHS)) TypeOfDiv;
	else
	static if( /+ (/ (/R> A (+> B X)) C) -> (/R> (/ A C) (+> B X)) +/
		is(T.Op == DivAR) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpDivAR!(OpDiv!(T.LHS, V), OpAddA!(T.RHS.LHS, T.RHS.RHS)) TypeOfDiv;
	else
	static if( /+ (/ C (/R> A (+> B X))) -> (*> (/ A C) (+> B X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == DivAR) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpMulA!(OpDiv!(V.LHS, T), OpAddA!(V.RHS.LHS, V.RHS.RHS)) TypeOfDiv;
	else
	static if( /+ (/ (/> A (+> B X)) C) -> (*> (/ C A) (+> B X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpMulA!(OpDiv!(V, T.LHS), OpAddA!(T.RHS.LHS, T.RHS.RHS)) TypeOfDiv;
	else
	static if( /+ (/ C (/> A (+> B X))) -> (/R> (/ C A) (+> B X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivAR!(OpDiv!(T, V.LHS), OpAddA!(V.RHS.LHS, V.RHS.RHS)) TypeOfDiv;
	else
	static if( /+ (/ (*> A (+> B X)) C) -> (*> (* C A) (+> B X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == AddA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpMulA!(OpMul!(V, T.LHS), OpAddA!(T.RHS.LHS, T.RHS.RHS)) TypeOfDiv;
	else
	static if( /+ (/ C (*> A (+> B X))) -> (/R> (* A C) (+> B X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == AddA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivAR!(OpMul!(V.LHS, T), OpAddA!(V.RHS.LHS, V.RHS.RHS)) TypeOfDiv;
	else
	static if( /+ (/ (/R> A (-> B X)) C) -> (/R> (/ A C) (-> B X)) +/
		is(T.Op == DivAR) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpDivAR!(OpDiv!(T.LHS, V), OpSubA!(T.RHS.LHS, T.RHS.RHS)) TypeOfDiv;
	else
	static if( /+ (/ C (/R> A (-> B X))) -> (*> (/ A C) (-> B X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == DivAR) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpMulA!(OpDiv!(V.LHS, T), OpSubA!(V.RHS.LHS, V.RHS.RHS)) TypeOfDiv;
	else
	static if( /+ (/ (/> A (-> B X)) C) -> (*> (/ C A) (-> B X)) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpMulA!(OpDiv!(V, T.LHS), OpSubA!(T.RHS.LHS, T.RHS.RHS)) TypeOfDiv;
	else
	static if( /+ (/ C (/> A (-> B X))) -> (/R> (/ C A) (-> B X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivAR!(OpDiv!(T, V.LHS), OpSubA!(V.RHS.LHS, V.RHS.RHS)) TypeOfDiv;
	else
	static if( /+ (/ (*> A (-> B X)) C) -> (*> (* C A) (-> B X)) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.Op == SubA) && is(T.RHS.LHS.DefP == Defined) && is(T.RHS.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpMulA!(OpMul!(V, T.LHS), OpSubA!(T.RHS.LHS, T.RHS.RHS)) TypeOfDiv;
	else
	static if( /+ (/ C (*> A (-> B X))) -> (/R> (* C A) (-> B X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.Op == SubA) && is(V.RHS.LHS.DefP == Defined) && is(V.RHS.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivAR!(OpMul!(T, V.LHS), OpSubA!(V.RHS.LHS, V.RHS.RHS)) TypeOfDiv;
	else
	static if( /+ (/ (/R> B X) A) -> (/R> (/ B A) X) +/
		is(T.Op == DivAR) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpDivAR!(OpDiv!(T.LHS, V), T.RHS) TypeOfDiv;
	else
	static if( /+ (/ A (/R> B X)) -> (*> (/ B A) X) +/
		is(T.DefP == Defined) &&
		is(V.Op == DivAR) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpMulA!(OpDiv!(V.LHS, T), V.RHS) TypeOfDiv;
	else
	static if( /+ (/ (*> B X) A) -> (*> (* A B) X) +/
		is(T.Op == MulA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpMulA!(OpMul!(V, T.LHS), T.RHS) TypeOfDiv;
	else
	static if( /+ (/ A (*> B X)) -> (/R> (* A B) X) +/
		is(T.DefP == Defined) &&
		is(V.Op == MulA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivAR!(OpMul!(T, V.LHS), V.RHS) TypeOfDiv;
	else
	static if( /+ (/ (/> B X) A) -> (*> (/ A B) X) +/
		is(T.Op == DivA) && is(T.LHS.DefP == Defined) && is(T.RHS.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpMulA!(OpDiv!(V, T.LHS), T.RHS) TypeOfDiv;
	else
	static if( /+ (/ A (/> B X)) -> (/R> (/ A B) X) +/
		is(T.DefP == Defined) &&
		is(V.Op == DivA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivAR!(OpDiv!(T, V.LHS), V.RHS) TypeOfDiv;
	else
	static if( /+ (/ A (+> B X)) -> (/R> A (+> B X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == AddA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivAR!(T, OpAddA!(V.LHS, V.RHS)) TypeOfDiv;
	else
	static if( /+ (/ A (-> B X)) -> (/R> A (-> B X)) +/
		is(T.DefP == Defined) &&
		is(V.Op == SubA) && is(V.LHS.DefP == Defined) && is(V.RHS.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivAR!(T, OpSubA!(V.LHS, V.RHS)) TypeOfDiv;
	else
	static if( /+ (/ A B) -> (/ A B) +/
		is(T.DefP == Defined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpDiv!(T, V) TypeOfDiv;
	else
	static if( /+ (/ A X) -> (/R> A X) +/
		is(T.DefP == Defined) &&
		is(V.DefP == UnDefined)
		 /+ no repeats +/
		) 
		alias OpDivAR!(T, V) TypeOfDiv;
	else
	static if( /+ (/ X A) -> (*> A X) +/
		is(T.DefP == UnDefined) &&
		is(V.DefP == Defined)
		 /+ no repeats +/
		) 
		alias OpMulA!(V, T) TypeOfDiv;
	else
	static if( /+ (/ X X) -> 1 +/
		is(T.DefP == UnDefined) &&
		is(V.DefP == UnDefined)
		 && is(T == V)
		) 
		alias Value!(1) TypeOfDiv;
	else
		{}
	static if(!is(TypeOfDiv))
		static assert(false, `unusable types used for Div: (/ ` ~ T.LispOf ~ ` `~ V.LispOf ~ `)` );
}

