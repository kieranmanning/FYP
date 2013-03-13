module CoreRep where



 {-- 
General thoughts on ext-core

BASIC FORMAT
(for "module name where main = do things")

%module main:mIdent

%data main:mIdent.dataIdent paramGuID = 
	{ ';'-seperated elemets }

FUNCTIONS

**Case of no params**
main:mIdent.funcId :: ghcPrimitiveType = return

**Case params**
main:mIdent.funcId :: %forall typeVar . typeSig =
	\ @ typeVarId'd (?) (paramGuID::typeVarId'd) -> output



 --}

 data JSTypes 
 	= JSNum
 	| JSString


{-- 

function idFunc that takes paramater 'param'
of type forall param . param -> param (ie. 
not a function (in the classic sense)). 'xm'
is the anonymous type var representing param.
returns param

"ifFunc, for all values of 'param', will take
a 'param' of some type xm and return 'param'"

idFunc :: forall param . param -> param =
	\ @ xm (param::xm) -> param

in JS...

function idFunc(a){
	return a;
}

discard all type info

what we need...

ifFunc :: ... = param::anything -> param

For reading core itself...

	base:GHCziNum.zX = integer ops where x denotes op

--}

 data JSFunc = ParamList JSExpr

 data JSObj = 

coreType2JsType :: CorePrimitve -> JSType 
coreType2JsType coreType = do
	case coreType of
		coreInt 	-> JSNum
		coreDouble 	-> JSNum
		coreFloat 	-> JSNum
		coreString 	-> JSSTring
		coreChar	-> JSString
		--_			-> TypeError

coreFunc2JsFunc :: CoreExpr -> JSFunc

core2js :: CoreProgram -> JSProgram
core2js 

-- %module main:ModuleName
--data MIdent 
--	= 



-- CoreSyn
data Expr b
  = Var	  Id
  | Lit   Literal
  | App   (Expr b) (Arg b)
  | Lam   b (Expr b)
  | Let   (Bind b) (Expr b)
  | Case  (Expr b) b Type [Alt b]	-- See #case_invariant#
  | Cast  (Expr b) Coercion
  | Tick  (Tickish Id) (Expr b)
  | Type  Type
  | Coercion Coercion
  deriving (Data, Typeable)

-- Var, Id etc. from their own modules

{--

	CoreProgram = [CoreBind] (... = [(NonRecBinding b (Expr b)) Var])

	ie. list of (Expr, Vars) basically

--}

data Coercion

data Literal	
	= Int 
	| String

-- | Binding, used for top level bindings in a module and local bindings in a @let@.
data Bind b 
	= NonRec b (Expr b)
	| Rec [(b, (Expr b))]
  	deriving (Data, Typeable)

type CoreProgram = [CoreBind]	-- See Note [CoreProgram]

-- | The common case for the type of binders and variables when
-- we are manipulating the Core language within GHC
type CoreBndr = Var
-- | Expressions where binders are 'CoreBndr's
type CoreExpr = Expr CoreBndr
-- | Argument expressions where binders are 'CoreBndr's
type CoreArg  = Arg  CoreBndr
-- | Binding groups where binders are 'CoreBndr's
type CoreBind = Bind CoreBndr
-- | Case alternatives where binders are 'CoreBndr's
type CoreAlt  = Alt  CoreBndr

type Id = Var