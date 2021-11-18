{
module Parser (parse) where

import Lexer
import BamAST
}

%name parseC Code
%tokentype  { PToken }
%error      { parseError }

%token
VAR    { MkToken _ (TVar v)      }
NUM    { MkToken _ (TNum n)      }
TRUE   { MkToken _ (TBool True)  }
FALSE  { MkToken _ (TBool False) }
PUSH   { MkToken _  TPush        }
ADD    { MkToken _  TAdd         }
MUL    { MkToken _  TMul         }
SUB    { MkToken _  TSub         }
EQ     { MkToken _  TEq          }
LE     { MkToken _  TLe          }
NOT    { MkToken _  TNot         }
AND    { MkToken _  TAnd         }
LOAD   { MkToken _  TFetch       }
STORE  { MkToken _  TStore       }
IF     { MkToken _  TBranch      }
LOOP   { MkToken _  TLoop        }
NOOP   { MkToken _  TNoop        }
'('    { MkToken _  TLParen      }
')'    { MkToken _  TRParen      }
','    { MkToken _  TComma       }
';'    { MkToken _  TSemiC       }

%%

Code
  : Instr ';' Code  { $1 : $3 }
  |                 { []      }

Instr
  : ZConst { $1 }
  | BConst { $1 }
  | Op     { $1 }
  | Mem    { $1 }
  | Flow   { $1 }

ZConst
  : PUSH NUM { IPush (CNum (tkNum $2)) }

BConst
  : PUSH TRUE  { IPush (CBool True)  }
  | PUSH FALSE { IPush (CBool False) }

Op
  : NOT { INot }
  | ADD { IAdd }
  | MUL { IMul }
  | SUB { ISub }
  | EQ  { IEq  }
  | LE  { ILe  }
  | AND { IAnd }

Mem
  : LOAD VAR { IFetch (tkVar $2) }
  | STORE VAR { IStore (tkVar $2) }

Flow
  : IF '(' Code ',' Code ')'     { IBranch $3 $5 }
  | LOOP   '(' Code ',' Code ')' { ILoop   $3 $5 }
  | NOOP                         { INoop         }

{
-- How to handle errors; Hint: we don't. Hint2: we should.
parseError :: [PToken] -> a
parseError [] =
  error "Unexpected end of file (perhaps a missing ;?)"
parseError (tk : _) =
  let (l, c) = tkPos tk in
  error $ "Parse error near token: " ++ show tk

parse :: String -> Code
parse = parseC . alexScanTokens
}
