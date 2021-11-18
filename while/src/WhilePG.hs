module WhilePG where

progA :: String
progA = "n := n + 2"

progB :: String
progB = "if (n > 0) do\n
    r := n\n
  else\n
    r := -n"

progC :: String
progC = "a := a - b\n
  b := a + b\n
  b := b - a\n
  a := b - a"

progD :: String
progD = "r := 0\n
  while (n != 0) {\n
    if (n < -2) do\n
      n := n - 2\n
    else {\n
      n := n - 1\n
      r:= 1\n
    }\n
  }"

progE :: String
progE = "while (true) n := n + 1"

progFact :: String
progFact = "if (n < 0) do {\n
    n := -n\n
    negative := true\n
  } else\n
    negative := false\n
  r := 1\n
  while (1 <= n) {\n
    r := n * r\n
    n := n - 1\n
  }\n
  if (negative = true) do\n
    r := -r"
