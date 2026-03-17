module Grammar where

import Language
import Data.Map (fromList, Map, assocs, member)
import Debug.Trace
import Data.List (groupBy)

-- helper function to convert disjunctions into separate rules
undisjunct :: String -> [String] -> [Production]
undisjunct _ [] = error "cry"
undisjunct name (h:[]) = [NonTerminal name h "epsilon"]
undisjunct name (h:t) = (NonTerminal name h "epsilon"):(undisjunct name t)

-- helper function to convert a non-terminal rule >= 2 into a rule containing at most 2 non-terminals
unconcat :: String -> [String] -> [Production]
unconcat _ [] = error "cry"
unconcat name (h:[]) = [NonTerminal name h "epsilon"]
unconcat name (h1:h2:[]) = [NonTerminal name h1 h2]
unconcat name (h1:h2:t) = 
    let newName = h1 ++ "_" ++ h2
    in [NonTerminal newName h1 h2] ++ (unconcat name (newName:t))

-- helper function to deal with optionals
unoptional :: String -> String -> [Production]
unoptional name rule = [NonTerminal name rule "epsilon", NonTerminal name "epsilon" "epsilon"] 

--helper function to deal with kleene star, false means there is no minimum, true means there is a minimum of one
--these are the only options in RFC 5322
unkleene :: String -> String -> Bool -> [Production]
unkleene nameStar name False = [NonTerminal nameStar "epsilon" "epsilon", 
                                NonTerminal nameStar name nameStar]
unkleene nameStar name True = [NonTerminal nameStar name "epsilon",
                               NonTerminal nameStar name nameStar]

epsilon :: Production
epsilon = Terminal "epsilon" CharRange {start = 0,end = 0}

addrSpec :: [Production]
addrSpec = unconcat "addr-spec" ["local-part", "at-sign", "domain"]

atSign :: Production
atSign = Terminal "at-sign" $ singletonRange '@'

localPart :: [Production]
localPart = undisjunct "local-part" ["dot-atom", "quoted-string", "obs-local-part"]

domain :: [Production]
domain = undisjunct "domain" ["dot-atom", "domain-literal", "obs-domain"]

optionalCFWS :: [Production]
optionalCFWS = unoptional "opt-cfws" "CFWS"

dotAtom :: [Production]
dotAtom = unconcat "dot-atom" ["opt-cfws", "dot-atom-text", "opt-cfws"]

kleeneOneAtext :: [Production]
kleeneOneAtext = unkleene "1*atext" "atext" True

dotAtext :: Production
dotAtext = NonTerminal "dot-atext" "dot" "1*atext"

kleeneDotAtext :: [Production]
kleeneDotAtext = unkleene "*dot-atext" "dot-atext" False

dotAtomText :: Production
dotAtomText = NonTerminal "dot-atom-text" "1*atext" "*dot-atext"

dot :: Production
dot = Terminal "dot" $ singletonRange '.'

atom :: [Production]
atom = unconcat "atom" ["opt-cfws", "1*atext", "opt-cfws"]

leftSquareBracket :: Production 
leftSquareBracket = Terminal "left-square-bracket" $ singletonRange '['

rightSquareBracket :: Production 
rightSquareBracket = Terminal "right-square-bracket" $ singletonRange ']'

optionalFWS :: [Production]
optionalFWS = unoptional "opt-fws" "FWS"

optionalFWSDtext :: Production 
optionalFWSDtext = NonTerminal "opt-fws-dtext"  "opt-fws" "dtext"

kleeneOptionalFWSDText :: [Production]
kleeneOptionalFWSDText = unkleene "*opt-fws-dtext" "opt-fws-dtext" False

domainLiteral :: [Production]
domainLiteral = unconcat "domain-literal" ["opt-cfws", "left-square-bracket", "*opt-fws-dtext", "opt-fws", "right-square-bracket", "opt-cfws"]

d33_90 :: Production
d33_90 = Terminal "%d33-90" CharRange {start = 33, end = 90 + 1}
d94_126 :: Production
d94_126 = Terminal "%d94-126" CharRange {start = 94, end = 126 + 1}

dtext :: [Production]
dtext = undisjunct "dtext" ["%d33-90", "%d94-126", "obs-dtext"]

obsDtext :: [Production]
obsDtext = undisjunct "obs-dtext" ["obs-NO-WS-CTL", "quoted-pair"]

optionalFWSQContent :: Production
optionalFWSQContent = NonTerminal "opt-fws-qcontent" "opt-fws" "qcontent"

kleeneOptionalFSWQContent :: [Production]
kleeneOptionalFSWQContent = unkleene "*opt-fws-qcontent" "opt-fws-qcontent" False

quotedString :: [Production]
quotedString = unconcat "quoted-string" ["opt-cfws", "DQUOTE", "*opt-fws-qcontent", "opt-fws", "DQUOTE", "opt-cfws"]

d33 :: Production
d33 = Terminal "%d33" CharRange {start = 33, end = 34}
d35_91 :: Production
d35_91 = Terminal "%d35-91" CharRange {start = 35, end = 91 + 1}
d93_126 :: Production
d93_126 = Terminal "%d93-126" CharRange {start = 93, end = 126 + 1}

qtext :: [Production]
qtext = undisjunct "qtext" ["%d33", "%d35-91", "%d93-126", "obs-NO-WS-CTL"]

qcontent :: [Production]
qcontent = undisjunct "qcontent" ["qtext", "quoted-pair"]

quotedPair :: [Production]
quotedPair = undisjunct "quoted-pair" ["backslash-VCHAR-WSP", "obs-qp"]

backslashVCHARWSP :: Production
backslashVCHARWSP = NonTerminal "backslash-VCHAR-WSP" "backslash" "VCHAR-WSP"

vcharWSP :: [Production]
vcharWSP = undisjunct "VCHAR-WSP" ["VCHAR", "WSP"]

backslash :: Production
backslash = Terminal "backslash" $ singletonRange '\\'

vchar :: Production
vchar = Terminal "VCHAR" $ CharRange {start = 33, end = 126}

atext :: [Production]
atext = undisjunct "atext" ["ALPHA", "DIGIT", "exclamation-mark", "hash", "dollar-sign", "percentage", "ampersand", "apostrophe", "star", "plus", "dash", "slash", "equals", "question-mark", "caret", "underscore", "backtick", "left-curly-brace", "vertical-bar", "right-curly-brace", "tilde"]

exclamationMark :: Production 
exclamationMark = Terminal "exclamation-mark" $ singletonRange '!'

hash :: Production 
hash = Terminal "hash" $ singletonRange '#'

dollarSign :: Production 
dollarSign = Terminal "dollar-sign" $ singletonRange '$'

percentage :: Production 
percentage = Terminal "percentage" $ singletonRange '%'

ampersand :: Production
ampersand = Terminal "ampersand" $ singletonRange '&'

apostrophe :: Production
apostrophe = Terminal "apostrophe" $ singletonRange '\''

star :: Production
star = Terminal "star" $ singletonRange '*'

plus :: Production
plus = Terminal "plus" $ singletonRange '+'

dash :: Production
dash = Terminal "dash" $ singletonRange '-'

slash :: Production
slash = Terminal "slash" $ singletonRange '/'

equals :: Production
equals = Terminal "equals" $ singletonRange '='

questionMark :: Production
questionMark = Terminal "question-mark" $ singletonRange '?'

caret :: Production
caret = Terminal "caret" $ singletonRange '^'

underscore :: Production
underscore = Terminal "underscore" $ singletonRange '_'

backtick :: Production
backtick = Terminal "backtick" $ singletonRange '`'

leftCurlyBrace :: Production
leftCurlyBrace = Terminal "left-curly-brace" $ singletonRange '{'

verticalBar :: Production
verticalBar = Terminal "vertical-bar" $ singletonRange '|'

rightCurlyBrace :: Production
rightCurlyBrace = Terminal "right-curly-brace" $ singletonRange '}'

tilde :: Production
tilde = Terminal "tilde" $ singletonRange '~'

ccontent :: [Production]
ccontent = undisjunct "ccontent" ["ctext", "quoted-pair", "comment"]

leftParenthesis :: Production
leftParenthesis = Terminal "left-parenthesis" $ singletonRange '('

rightParenthesis :: Production
rightParenthesis = Terminal "right-parenthesis" $ singletonRange ')'

optionalFWSContent :: Production
optionalFWSContent = NonTerminal "opt-fws-ccontent" "opt-fws" "ccontent"

kleeneOptionalFWSContent :: [Production]
kleeneOptionalFWSContent = unkleene "*opt-fws-ccontent" "opt-fws-ccontent" False 

comment :: [Production]
comment = unconcat "comment" ["left-parenthesis", "*opt-fws-ccontent", "opt-fws", "right-parenthesis"]

optionalFWSComment :: Production 
optionalFWSComment = NonTerminal "opt-fws-comment" "opt-fws" "comment"

kleeneOptionalFWSComment :: [Production]
kleeneOptionalFWSComment = unkleene "1*opt-fws-comment" "opt-fws-comment" True 

kleeneOptFWSCommentOptionalFWS :: Production
kleeneOptFWSCommentOptionalFWS = NonTerminal "1*opt-fws-comment-opt-fws" "1*opt-fws-comment" "opt-fws"

cFWS :: [Production]
cFWS = undisjunct "CFWS" ["1*opt-fws-comment-opt-fws", "FWS"]

starWSP :: [Production]
starWSP = unkleene "*WSP" "WSP" False 

starWSPCRLF :: Production
starWSPCRLF = NonTerminal "*wsp-crlf" "*WSP" "CRLF"

optionalStarWSPCRLF :: [Production]
optionalStarWSPCRLF = unoptional "opt-*wsp-crlf" "*wsp-crlf"

oneStarWSP :: [Production]
oneStarWSP = unkleene "1*WSP" "WSP" True 

optionalStarWSPCRLFOneStarWSP :: Production
optionalStarWSPCRLFOneStarWSP = NonTerminal "opt-*wsp-crlf-1*wsp" "opt-*wsp-crlf" "1*WSP"

fWS :: Production
fWS = NonTerminal "FWS" "opt-*wsp-crlf-1*wsp" "obs-FWS"

d33_39 :: Production
d33_39 = Terminal "%d33-39" CharRange {start = 33, end = 39  + 1}
d42_91 :: Production
d42_91 = Terminal "%d42-91" CharRange {start = 42, end = 91  + 1}

ctext :: [Production]
ctext = undisjunct "ctext" ["%d33-39", "%d42-91", "%d93-126", "obs-NO-WS-CTL"]

upperCaseLetters :: Production
upperCaseLetters = Terminal "%x41-5A" $ CharRange {start = 65, end = 90 + 1}

lowerCaseLetters :: Production
lowerCaseLetters = Terminal "%x61-7A" $ CharRange {start = 97, end = 122 + 1}

alpha :: [Production]
alpha = undisjunct "ALPHA" ["%x41-5A", "%x61-7A"]

digit :: Production
digit = Terminal "DIGIT" $ CharRange {start = 48, end = 57 + 1}

dquote :: Production
dquote = Terminal "DQUOTE" $ singletonRange '\"'

periodAtom :: Production
periodAtom = NonTerminal "period-atom" "dot" "atom"

kleenePeriodAtom :: [Production]
kleenePeriodAtom = unkleene "*period-atom" "period-atom" False 

obsDomain :: Production
obsDomain = NonTerminal "obs-domain" "atom" "*period-atom"

periodWord :: Production
periodWord = NonTerminal "period-word" "dot" "period-word"

kleenePeriodWord :: [Production]
kleenePeriodWord = unkleene "*period-word" "period-word" False

obsLocalPart :: Production
obsLocalPart = NonTerminal "obs-local-part" "word" "*period-word"

d0 :: Production
d0 = Terminal "%d0" $ CharRange {start = 0, end = 1}

obsQPPart :: [Production]
obsQPPart = undisjunct "obs-qp-part" ["%d0", "obs-NO-WS-CTL", "%x0A", "%x0D"]

obsQP :: Production
obsQP = NonTerminal "obs-qp" "backslash" "obs-qp-part"

d1_8 :: Production
d1_8 = Terminal "%d1-8" CharRange {start = 1, end = 8  + 1}

d11_12 :: Production
d11_12 = Terminal "%d11-12" CharRange {start = 11, end = 12  + 1}

d14_31 :: Production
d14_31 = Terminal "%d14-31" CharRange {start = 14, end = 31  + 1}

d127 :: Production
d127 = Terminal "%d127" CharRange {start = 127, end = 127  + 1}


obsNOWSCTL :: [Production]
obsNOWSCTL = undisjunct "obs-NO-WS-CTL" ["%d1-8", "%d11-12", "%d14-31", "%d127"]

cRLFOneStarWSP :: Production
cRLFOneStarWSP = NonTerminal "crlf-1*wsp" "CRLF" "1*WSP"

starcRLFOneStarWSP :: [Production]
starcRLFOneStarWSP = unkleene "*crlf-1*wsp" "crlf-1*wsp" False

obsFWS :: Production
obsFWS = NonTerminal "obs-FWS" "1*WSP" "*crlf-1*wsp"

word :: [Production]
word = undisjunct "word" ["atom", "quoted-string"]

x20 :: Production
x20 = Terminal "%x20" $ CharRange {start = 32, end = 32 + 1}

x09 :: Production
x09 = Terminal "%x09" $ CharRange {start = 9, end = 9 + 1}

wsp :: [Production]
wsp = undisjunct "WSP" ["%x20", "%x09"]

x0D :: Production
x0D = Terminal "%x0D" $ CharRange {start = 13, end = 13 + 1}

x0A :: Production
x0A = Terminal "%x0A" $ CharRange {start = 10, end = 10 + 1}

crlf :: Production 
crlf = NonTerminal "CRLF" "%x0D" "%x0A"


allProductions ::  [Production]
allProductions =  [epsilon] 
    ++ addrSpec
    ++ [atSign] 
    ++ localPart
    ++ domain
    ++ optionalCFWS
    ++ dotAtom
    ++ kleeneOneAtext
    ++ [dotAtext]
    ++ kleeneDotAtext
    ++ [dotAtomText]
    ++ [dot]
    ++ atom
    ++ [leftSquareBracket]
    ++ [rightSquareBracket]
    ++ optionalFWS
    ++ [optionalFWSDtext]
    ++ kleeneOptionalFWSDText
    ++ domainLiteral
    ++ [d33_90]
    ++ [d94_126]
    ++ dtext
    ++ obsDtext
    ++ [optionalFWSQContent]
    ++ kleeneOptionalFSWQContent
    ++ quotedString
    ++ [d33]
    ++ [d35_91]
    ++ [d93_126]
    ++ qtext
    ++ qcontent
    ++ quotedPair
    ++ [backslashVCHARWSP]
    ++ [backslash]
    ++ vcharWSP
    ++ [vchar]
    ++ atext
    ++ [exclamationMark]
    ++ [hash]
    ++ [dollarSign]
    ++ [percentage]
    ++ [ampersand]
    ++ [apostrophe]
    ++ [plus]
    ++ [star] 
    ++ [dash]
    ++ [slash]
    ++ [equals]
    ++ [questionMark]
    ++ [caret]
    ++ [underscore]
    ++ [backtick]
    ++ [leftCurlyBrace]
    ++ [verticalBar]
    ++ [rightCurlyBrace]
    ++ [tilde]
    ++ ccontent
    ++ [leftParenthesis]
    ++ [rightParenthesis]
    ++ [optionalFWSContent]
    ++ kleeneOptionalFWSContent
    ++ comment
    ++ [optionalFWSComment]
    ++ kleeneOptionalFWSComment
    ++ [kleeneOptFWSCommentOptionalFWS]
    ++ cFWS
    ++ starWSP
    ++ [starWSPCRLF]
    ++ optionalStarWSPCRLF
    ++ oneStarWSP
    ++ [optionalStarWSPCRLFOneStarWSP]
    ++ [fWS]
    ++ [d33_39]
    ++ [d42_91]
    ++ ctext
    ++ [upperCaseLetters]
    ++ [lowerCaseLetters]
    ++ alpha
    ++ [digit]
    ++ [dquote]
    ++ [periodAtom]
    ++ kleenePeriodAtom
    ++ [obsDomain]
    ++ [periodWord]
    ++ kleenePeriodWord
    ++ [obsLocalPart]
    ++ [d0]
    ++ obsQPPart
    ++ [obsQP]
    ++ [d1_8]
    ++ [d11_12]
    ++ [d14_31]
    ++ [d127]
    ++ obsNOWSCTL
    ++ [cRLFOneStarWSP]
    ++ starcRLFOneStarWSP
    ++ [obsFWS]
    ++ word 
    ++ [x20]
    ++ [x09]
    ++ [wsp]
    ++ [x0D]
    ++ [x0A]
    ++ [crlf]

productions :: Map String [Production]
productions = 
    let prodPairs = (map (\(x::Production)-> case x of 
                                    NonTerminal s _ _ -> (s, x)
                                    Terminal s _ -> (s, x)) allProductions)
    in fromList (productionsHelper (groupBy (\a b -> fst a == fst b) prodPairs))

productionsHelper :: [[(String, Production)]] -> [(String, [Production])]
productionsHelper [] = []
productionsHelper (h:t) = let s = fst (head h) in (s, map (\x -> snd x) h):(productionsHelper t)

cfg :: CFG 
cfg = CFG {prods = productions, startRule = "addr-spec", budgets = fromList [("local-part", Finite 2), ("domain", Finite 3)]}

checkConsistent :: Map String [Production] -> Bool 
checkConsistent prods = checkConsistentHelper (assocs prods) prods

checkConsistentHelper :: [(String, [Production])] -> Map String [Production] -> Bool 
checkConsistentHelper [] _ = True
checkConsistentHelper ((_, s_prods):t) prods = (checkConsistentHelperHelper s_prods prods) && checkConsistentHelper t prods

checkConsistentHelperHelper :: [Production] -> Map String [Production] -> Bool 
checkConsistentHelperHelper [] _ = True
checkConsistentHelperHelper ((Terminal _ _):t) prods = checkConsistentHelperHelper t prods
checkConsistentHelperHelper ((NonTerminal _ rule1 rule2):t) prods = if member rule1 prods then 
                                                                    (if member rule2 prods then 
                                                                        checkConsistentHelperHelper t prods 
                                                                    else trace rule2 False)
                                                                    else trace rule1 False