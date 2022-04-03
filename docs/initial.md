# TKOM — Projekt

Filip Lew

## Opis projektu

Projekt ma na celu stworzenie interpretera prostego języka implementującego tzw. *pattern matching* oraz we wszystkie typowe konstrukcje języków imperatywnych. Język czerpie inspirację z języka Rust, choć jest względem niego mocno uproszczony.

## Założenia

- język posiada następujące typy prymitywne: liczby całkowite, liczby zmiennoprzecinkowe, wartości logiczne i ciągi znaków,
- język posiada dwa rodzaje typów złożonych: struktury (typy iloczynowe) oraz enumeracje (typy sumowe, "tagowane unie"),
- w języku można definiować funkcje:
  - funkcje można definiować tylko na najwyższym poziomie kodu. Nie są wspierane *closures*.
  - nie można definiować kodu poza funkcją,
  - wymagana jest funkcja `main`, będąca punktem wejścia.
- zmienne są modyfikowalne (*mutable*) lub nie. Domyślnym stanem jest brak modyfikowalności.
- zmiennym, argumentom i wartościom zwracanym trzeba zawsze nadać typ. Inferencja typów nie jest wspierana.
- wspierane konstrukcje przepływu sterowania to `if`, `match`, `for` i `while`,
- `if` oraz `match` to wyrażenia: ewaluują do wartości, którą można przypisać do zmiennej,
- język jest silnie i statycznie typowany,
- nie ma niejawnych konwersji między typami, wszystkich konwersji musi dokonywać programista przy pomocy operatora `as`,
- większość operatorów arytmetyczno-relacyjnych jest zdefiniowana jedynie dla typów liczbowych, wyjątkiem jest operator `+`, który jest zdefiniowany również dla typu `string`,
- operatory działają jedynie dla operandów jednego typu, nie można zrobić np. `2 / 1.5` bez rzutowania,
- pattern matching nie wspiera destrukturyzacji struktur.

## Wymagania

### Funkcjonalne

- wykonywanie kodu zawartego w strumieniu wejściowym (pochodzącym np. z pliku czy `stdin`),
- weryfikacja poprawności składniowej oraz semantycznej (np. czy wszystkie ramiona `match` zwracają ten sam typ), przed wykonaniem programu (albo pewnej jego części),
- możliwość definiowania struktur typowych dla języków imperatywnych (funkcje, przepływ sterowania, zmienne, typy) ze szczególnym uwzględnieniem konstrukcji `match`,
- posiadanie podstawowego I/O.

### Niefunkcjonalne

- żadna kombinacja argumentów wejściowych i zawartości strumieni nie powinna powodować "brudnego" zakończenia się interpretera — zamiast tego, czytelna wiadomość o błędzie i ew. możliwościach rozwiązania go powinna zostać wypisana.

## Sposób uruchomienia

W najprostszym przypadku, interpreter powinno uruchamiać się, podając mu jako argument plik do wczytania, bądź `--`, aby wczytać kod z `stdin`. Na przykład:

```
./minirust prog.mrs
```

albo:

```
cat prog.mrs | ./minirust --
```

Możliwe, że w dalszych etapach pojawią się dodatkowe flagi, pozwalające na dostosowanie pewnych zachowań interpretera, jak np. tryb debugowania.

Wyjściem interpretera jest, oczywiście, wyjście wykonywanego przez niego programu, ew. przerywane komunikatami diagnostycznymi w trybie debugowania, albo komunikat błędu.

W przypadku zakończenia się z błędem, interpreter zwróci niezerową wartość wyjściową, aby ułatwić korzystanie z niego w skryptach.

## Opis realizacji

Działanie interpretera będzie składało się z kilku etapów. Wyjście każdego etapu jest wejściem kolejnego. Etapy są realizowane przez moduły opisane poniżej.

### Wejście

Najprostszy moduł, który polega na otwarciu pliku lub innego źródła danych i zwróceniu abstrakcyjnego interfejsu strumienia do niego.

### Lekser

Zadaniem leksera jest rozbicie strumienia znaków na pojedyncze tokeny składniowe. W tym celu będzie on przyrostowo czytał znaki ze strumienia, aż uda mu się skonstruować token. Wtedy token ten zostanie przekazany do parsera, a pobieranie znaków zostanie wstrzymane do momentu, gdy parser poprosi o więcej tokenów. Lekser powinien mieć również możliwość zwrócenia dwuwymiarowej pozycji w tekście początku obecnego tokenu (linia i kolumna).

### Parser

Zadaniem parsera jest przekształcenie płaskiej, liniowej struktury prymitywnych tokenów w drzewiastą reprezentację bardziej wysokopoziomowych elementów (abstrakcyjne drzewo składniowe). Jego zadaniem jest również sprawdzanie poprawności kodu źródłowego z gramatyką, oraz zgłaszanie odpowiednich błędów, jeśli tak nie jest.

### Weryfikacja semantyki

Zadaniem tego modułu jest sprawdzenie pewnych ogólnych reguł semantycznych języka, jak np. posiadanie definicji przez wszystkie używane identyfikatory, zgodność typów argumentów z typami parametrów, czy wszystkie możliwości zostały wyczerpane w wyrażeniu `match`, itp. Ze względu na konieczność posiadania całego drzewa składniowego do weryfikacji niektórych reguł, takich jak poprawność identyfikatorów funkcji w przypadku cyklicznych odwołań, moduł ten rozpocznie swoje wykonywanie dopiero wtedy, gdy parser zakończy pracę.

### Interpreter

Zadaniem interpretera jest przejście po drzewie składniowym, wykonując po drodze zawarte w nim instrukcje i aktualizując stan wewnętrzny. Ten moduł również rozpocznie swoje wykonanie dopiero wtedy, gdy weryfikacja semantyki się zakończy, by jak najwcześniej poinformować użytkownika o problemach w jego programie.

### Poboczne moduły

- **obsługa błędów** — zajmuje się wypisaniem czytelnej i jak najbardziej pomocnej użytkownikowi informacji o błędzie. Powinna przyjmować rodzaj błędu oraz metadane na jego temat (np. pozycja tokenu w tekście).
- **mapa zmiennych** — mapuje nazwy zmiennych do ich zawartości. Na początku jest wypełniona definicjami z biblioteki standardowej.

## Testowanie

Testowanie najprawdopodobniej będzie wykonywane przy pomocy wbudowanego systemu testowania Rust, który wywoła funkcję wejściową interpretera na pewnym zestawie niewielkich fragmentów kodu, mających za zadanie sprawdzać pojedyncze (lub jak najmniejsze zestawy) funkcjonalności interpretera.

Jeśli to podejście będzie sprawiało trudności, zastosowany zostanie zamiast niego zwykły skrypt shellowy, wywołujący interpreter jako polecenie.

Poza tym, będą jeszcze testy jednostkowe, testujące pojedyncze funkcje wchodzące w skład interpretera (np. przekazanie fragmentu tekstu do jednej z funkcji leksera i sprawdzenie, czy zwraca poprawny token).

## Wykorzystane narzędzia

- język Rust (najnowsza wersja w chwili oddawania projektu),
- standardowe narzędzia budowania Rust (`cargo` etc.),
- biblioteki — możliwe, że żadne, ew. jakieś niewielkie utilities do zadań takich jak przetwarzanie argumentów wiersza poleceń

## Przykładowy kod

```
// comment

// constant
const u64 SOME_CONSTANT = 1;

// struct
struct SomeStruct {
    // some primitive types
    i64 field1,
    u64 field2,
    f64 field3,
    string field4, // trailing comma should work
}

// enum (tagged union)
enum SomeEnum {
    EmptyVariant,
    PrimitiveVariant(u64),
    StructVariant(SomeStruct),
}

// function definition
// main - entry point, required
fn main() {
    // immutable variable
    u64 var1 = 1;

    // mutable variable
    mut u64 var2 = 1;

    // if expression
    string var3 = if var1 == 1 {
        "var1 is 1"
    } else {
        "var1 is something else"
    }

    // for loop
    for u64 i in 0..10 {
        print("index is ");
        println(i);
    }

    // for loop (inclusive end of range)
    for u64 i in 0..=10 {
        print("index is ");
        println(i);
    }

    // while loop
    while var2 < 10 {
        var2 += 1;
    }

    SomeEnum var3 = EmptyVariant;

    // match expression
    match var3 {
        // simple match
        EmptyVariant => 1,

        // match with variable binding
        PrimitiveVariant(inner_val) => inner_val,

		StructVariant(str) => {
			// block of code in an arm
			println(str.field1);
		}
    }
    
    match var1 {
    	1 | 2 => "1 or 2",
    	3..=5 => "from 3 to 5, inclusive",
    	_ => "all the rest",
    }
    
    // math expression
    println(2 + 2 * 2 == 6 && 5 - 4 / 2 == 3);
}
```

## Gramatyka

Uwagi do gramatyki:

- składnia gramatyki to W3C EBNF, gdyż wydaje się być najbardziej czytelna, oraz ma wsparcie dla zakresów znaków,
- ze względu na trudności w zapisaniu pełnej definicji literału ciągu znaków, poniżej zamieszczona jest uproszczona, nieuwzględniająca sekwencji specjalnych jak `\n` czy `\"`.

```
program ::= (function_def | struct_def | enum_def | const_def | comment)*;
comment ::= '//' #'[^\n]';

var_def         ::= 'mut'? identifier assignment_stmt ';';
const_def       ::= 'const' identifier assignment_stmt ';';
enum_def        ::= 'enum' identifier '{' (identifier '(' identifier ')' ',')* (identifier '(' identifier ')')? '}';
struct_def      ::= 'struct' identifier '{' (identifier identifier ',')* (identifier identifier)? '}';
function_def    ::= 'fn' '(' (identifier identifier ',')* (identifier identifier)? ')' ('->' identifier)? block_expr;

statement       ::= var_def | const_def | assignment_stmt | return_stmt | for_stmt | while_stmt | expression ';';
while_stmt      ::= 'while' expression block_expr;
for_stmt        ::= 'for' identifier identifier 'in' range block_expr;
return_stmt     ::= 'return' expression ';';
assignment_stmt ::= identifier assignment_op expression ';';

block_expr  ::= '{' (statement | comment)* (expression | comment)? '}';
match_expr  ::= 'match' identifier '{' (match_arm ',')* match_arm '}';
if_expr     ::= 'if' expression block_expr ('else' 'if' expression block_expr)* ('else' block_expr)?;

match_arm   ::= pattern '=>' expression;
pattern     ::= identifier ('(' pattern ')')? | literal_range | literal ('|' literal)*;

range ::= value ('..' | '..=') value;
literal_range ::= integer ('..' | '..=') integer;

value               ::= identifier | literal | function_call | member_access | '(' expression ')';
expression          ::= or_expr | if_expr | match_expr | block_expr;
or_expr             ::= and_expr (or_op and_expr)*;
and_expr            ::= relational_expr (and_op relational_expr)*;
relational_expr     ::= additive_expr ((relational_op | equality_op) additive_expr)*;
additive_expr       ::= multiplicative_expr (additive_op multiplicative_expr)*;
multiplicative_expr ::= as_expr (multiplicative_op as_expr)*;
as_expr             ::= unary_expr ("as" identifier)?;
unary_expr          ::= not_op* value;
member_access       ::= identifier '.' identifier;
function_call       ::= identifier '(' (expression ',')* expression? ')';

not_op              ::= '!';
or_op               ::= '||';
and_op              ::= '&&';
relational_op       ::= '>' | '<' | '>=' | '<=';
equality_op         ::= '==' | '!=';
additive_op         ::= '+' | '-';
multiplicative_op   ::= '*' | '/' | '%';
assignment_op       ::= '=' | '+=' | '-=' | '*=' | '/=' | '%=' | '&&=' | '||=';

literal     ::= number | string;
number      ::= integer | float;
string      ::= '"' character* '"';
float       ::= (integer '.' digit*) | ('.' digit+);
integer     ::= ('+' | '-') nonzero_digit digit*;
identifier  ::= (letter | '_') (letter | digit | '_')*;

character       ::= #'[^"]';
nonzero_digit   ::= #'[1-9]';
digit           ::= #'[0-9]';
letter          ::= #'[a-zA-Z]';
```
