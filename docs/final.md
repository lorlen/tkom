# TKOM - Dokumentacja końcowa

Filip Lew

## Opis projektu

Projekt jest interpreterem prostego języka implementującego tzw. *pattern matching* oraz we wszystkie typowe konstrukcje języków imperatywnych. Język czerpie inspirację z języka Rust, choć jest względem niego mocno uproszczony.

## Założenia

- język posiada następujące typy prymitywne: liczby całkowite, liczby zmiennoprzecinkowe, wartości logiczne i ciągi znaków,
- język posiada dwa rodzaje typów złożonych: struktury (typy iloczynowe) oraz enumeracje (typy sumowe, "tagowane unie"),
- wszystkie typy są przekazywane poprzez wartość, poza strukturami i ciągami znaków, które przekazywane są przez referencję,
- w języku można definiować funkcje:
  - funkcje można definiować tylko na najwyższym poziomie kodu. Nie są wspierane *closures*.
  - nie można definiować kodu poza funkcją,
  - wymagana jest funkcja `main`, będąca punktem wejścia.
- wspierane konstrukcje przepływu sterowania to `if`, `match`, `for` i `while`,
- `if` oraz `match` to wyrażenia: ewaluują do wartości, którą można przypisać do zmiennej,
- język jest typowany w "mieszany" sposób — zmienne są dynamicznie typowane, ale do pól struktur i wariantów enumeracji można przypisywać jedynie wartości zadanego typu,
- nie ma niejawnych konwersji między typami, wszystkich konwersji musi dokonywać programista przy pomocy operatora `as`,
- większość operatorów arytmetycznych jest zdefiniowana jedynie dla typów liczbowych, wyjątkiem jest operator `+`, który jest zdefiniowany również dla typu `string`,
- operatory działają jedynie dla operandów jednego typu, nie można zrobić np. `2 / 1.5` bez rzutowania,
- pattern matching nie wspiera destrukturyzacji struktur,
- język wspiera zagnieżdżone zakresy — każdy blok tworzy nowy zakres, w którym umieszczane są zmienne deklarowane wewnątrz tego bloku, i zakres ten jest niszczony po wyjściu z bloku

## Wymagania

### Funkcjonalne

- wykonywanie kodu zawartego w strumieniu wejściowym (pochodzącym np. z pliku czy `stdin`),
- weryfikacja poprawności składniowej oraz semantycznej (np. czy wszystkie ramiona `match` zwracają ten sam typ), przed wykonaniem programu (albo pewnej jego części),
- możliwość definiowania struktur typowych dla języków imperatywnych (funkcje, przepływ sterowania, zmienne, typy) ze szczególnym uwzględnieniem konstrukcji `match`,
- posiadanie podstawowego I/O.

### Niefunkcjonalne

- żadna kombinacja argumentów wejściowych i zawartości strumieni nie powinna powodować "brudnego" zakończenia się interpretera — zamiast tego, czytelna wiadomość o błędzie i ew. możliwościach rozwiązania go powinna zostać wypisana.

## Sposób budowania i uruchomienia

### Budowanie

Aby zbudować interpreter, należy w katalogu głównym projektu wydać polecenie:

```
cargo build --release
```

Zbudowany plik wykonywalny interpretera będzie znajdował się pod ścieżką `./target/release/tkom` (zakładany jest system uniksowy, projekt nie był testowany pod systemem Windows).

W najprostszym przypadku, interpreter powinno uruchamiać się, podając mu jako argument plik do wczytania, bądź `-`, aby wczytać kod z `stdin`. Na przykład:

```
./tkom prog.txt
```

albo:

```
cat prog.txt | ./tkom -
```

Wyjściem interpretera jest wyjście wykonywanego przez niego programu i/lub komunikat błędu.

W przypadku zakończenia się z błędem, interpreter zwróci niezerową wartość wyjściową, aby ułatwić korzystanie z niego w skryptach.

## Opis implementacji

Działanie interpretera składa się z kilku etapów. Wyjście każdego etapu jest wejściem kolejnego. Etapy są realizowane przez moduły opisane poniżej.

### Wejście

Najprostszy moduł, który polega na otwarciu pliku lub innego źródła danych i zwróceniu abstrakcyjnego interfejsu strumienia do niego. Realizowany jest przez bibliotekę `utf8-read`.

### Obsługa błędów

Zajmuje się wypisaniem czytelnej i jak najbardziej pomocnej użytkownikowi informacji o błędzie. Przyjmuje rodzaj błędu oraz metadane na jego temat (np. pozycja tokenu w tekście).

### Lekser

Zadaniem leksera jest rozbicie strumienia znaków na pojedyncze tokeny składniowe. W tym celu czyta on znaki ze strumienia, próbując dopasować je do kolejnych reguł, aż uda mu się skonstruować token. Wtedy token ten zostanie przekazany do parsera, a pobieranie znaków zostanie wstrzymane do momentu, gdy parser poprosi o więcej tokenów. W przypadku porażki, lekser zgłasza odpowiedni błąd, oznaczony pozycją w tekście, w której wystąpił problem.

Lekser jest iteratorem języka Rust, dzięki czemu można w idiomatyczny sposób operować na generowanym przez niego strumieniu tokenów. Sam token jest strukturą zawierającą `TokenKind`, który jest enumeracją języka Rust i zawiera zarówno informację o typie tokenu, jak i możliwe dodatkowe dane w przypadku złożonych tokenów takich jak liczby czy ciągi znaków, oraz pozycję w strumieniu.

### Parser

Parser konsumuje strumień tokenów pochodzących z leksera i na jego podstawie tworzy drzewo programu, będące reprezentacją operacji wykonywanych przez program i ich kolejności. Parser, podobnie jak lekser, jest przyrostowy, i czyta po jednym tokenie w miarę budowy drzewa. Powstałe drzewo programu ma strukturę ułatwiającą jego wykonanie.

Drzewo programu jest złożoną konstrukcją, w skład której wchodzą 24 struktury i 14 enumeracji języka Rust. Niektóre z nich mogą rekurencyjnie zawierać same siebie. Struktury są konkretnymi elementami składni, a enumeracje wyrażają zestawy możliwych alternatyw, jakie mogą występować w tych samych miejscach drzewa składniowego.

### Interpreter

Interpreter przechodzi po drzewie składniowym, wykonując zawarte w nim informacje zgodnie z semantyką języka. Interpreter weryfikuje również niezmienniki języka, takie jak poprawność typów, czy konieczność wykonywania pewnych operacji w pewnym kontekście. Interpreter jest oparty o wzorzec wizytatora, rekurencyjnie zagłębiający się w drzewo.

W tym module zawarte są również struktury zawierające stan wewnętrzny interpretera — takie jak stos wywołań, stos zakresów zmiennych oraz flagi kontroli przepływu sterowania.

### Biblioteka standardowa

Biblioteka standardowa zawiera kilka funkcji przydatnych do działania języka, ale które są niemożliwe do napisania korzystając z samego tego języka.

## Testowanie

Testowanie jest wykonywane za pomocą wbudowanego systemu testowania Rust — zdefiniowane są funkcje oznaczone atrybutem `#[test]`. Podejście do testowania się nieznacznie różni pomiędzy modułami:

- lekser — obiekt leksera jest tworzony zarówno na krótkich fragmentach tekstu zawierających pojedyncze tokeny, jak i na całym przykładowym fragmencie kodu zapisanym w repozytorium.
- parser — obiekt parsera przekazywany jest lekser stworzony na niewielkich fragmentach kodu zawierających minimalny poprawny program — jedna funkcja zawierająca fragment składni, który ma być przetestowany w danej funkcji. Ze względu na silne zależności między fragmentami parsera nie jest do końca możliwe przetestowanie jednostkowe pojedynczych funkcji.
- interpreter — do obiektu interpretera przekazywane są sparsowane drzewa krótkich programów, mające na celu przetestować pewną konkretną funkcjonalność języka.

## Opis języka

Język zaimplementowany w projekcie jest językiem imperatywnym, którego szczególną funkcjonalnością jest wyrażenie `match`, służące do dopasowywania wartości do wzorców, i podejmowania decyzji, do jakiego bloku kodu wejść. Elementy języka, ich semantyka i przykłady kodu opisane są poniżej.

### Semantyka

* język jest "mieszanie" typowany — zmienne nie mają ustalonego typu, można przypisywać do nich dowolne wartości, ale pola struktur i warianty enumeracji muszą zawierać określone typy.
* wszystkie wartości poza ciągami znaków i strukturami są przekazywane przez wartość. Ciągi znaków i struktury są przekazywane przez referencję.
* Zmienne są widoczne jedynie wewnątrz bloku w jakim zostały zdefiniowane (przez słowo kluczowe `let`).

### Elementy najwyższego poziomu (top-level)

#### Definicje stałych

Stałe są nazwanymi wartościami literalnymi (bądź wynikami wyrażeń na wartościach literalnych). Nie można ich modyfikować, jedynie odczytać. Stałe, podobnie jak zmienne, nie mają określonego typu, można przypisać do nich wartość dowolnego typu.

```
const GRAVITY = 9.81;
```

#### Definicje typów

Dostępne są dwa rodzaje definicji typów — struktury i enumeracje. Struktury są typami iloczynowymi, zawierają jednocześnie wszystkie elementy zdefiniowane w nich. Struktury są przekazywane przez referencję, ze względu na ich możliwy duży rozmiar.

```
struct Point {
	float x,
	float y
}
```

Enumeracje są typami sumowymi, zwanymi też uniami. Ich wartością jest dokładnie jedna z wypisanych w definicji możliwości — wariantów. Warianty mogą opcjonalnie zawierać dane, różne dla każdego wariantu.

```
enum MaybeInt {
	Some(int),
	None
}
```

Enumeracje przekazywane są przez referencję, jednak mogą zawierać w sobie referencję do jakiegoś innego typu.

#### Funkcje

Funkcje są nazwanymi blokami kodu, mogące przyjmować argumenty i zwracać wartości. Są one jedynym miejscem, w jakim logika programu może być definiowana. W każdym programie musi być zdefiniowana funkcja `main`, będąca punktem wejścia do programu.

```
fn something() {
	return 1;
}
```

Każda funkcja posiada blok instrukcji, definiujący jej logikę.

### Statementy

Są to instrukcje wykonujące pewną czynność, ale nie będące wartością same w sobie.

#### `if`

`if` może być zarówno statementem, jak i wyrażeniem, w zależności od miejsca, w którym się znajduje. Jest to sekwencja bloków i przypisanych do nich warunków, które są po kolei sprawdzane, i wykonywany jest pierwszy blok, którego warunek jest prawdziwy. Ostatni blok `else` może nie mieć warunku, wtedy jest zawsze wykonywany, gdy poprzednie warunki się nie powiodą.

```
if a > b {
	println("a greater than b");
} else if a == b {
	println("a equal b");
} else {
	println("a less than b");
}
```

#### `match`

`match`, podobnie jak `if`, może być wyrażeniem. Jest to konstrukcja dopasowywania wzorca, która może mieć kilka tzw. *ramion*, które są parami (wzór, blok). Gdy uda się dopasować wartość do wzorca, wykonywany jest blok stojący za tym wzorcem, i kończone jest wykonywanie statementu `match`. Dostępne są cztery formy wzorca:

* literał lub zakres — wartość jest dopasowana, jeśli jest równa literałowi lub mieści się w zakresie,
* wariant enumeracji — wartość jest dopasowana, jeśli wartość jest podanym wariantem enumeracji i wartość wewnątrz spełnia wzór podany w nawiasach,
* przypisanie — wzór jest bezwarunkowo akceptowany, a wartość jest przypisywana do zmiennej o podanej nazwie,
* *catch-all* — wzór jest bezwarunkowo akceptowany.

Pierwsze dwa rodzaje wzorca mogą występować w alternatywach oddzielanych `|`. Wzorzec jest akceptowany, gdy dowolna alternatywa jest akceptowana.

```
match a {
	1 | 3 .. 5 | 7 -> 0,
	SomeEnum.SomeVariant(1 | 2) -> 1,
	b -> 3,
	_ -> 4,
}
```

#### `while`

Pętla działająca, dopóki warunek jest prawdziwy.

```
while is_running {
	do_something()
}
```

#### `for`

Pętla wykonująca się określoną liczbę razy, i przypisująca iterator do zmiennej.

```
for i in range 1 ..= 10 {
	println("index is ", i);
}
```

#### Definicja zmiennej

Tworzy zmienną w obecnym zakresie, i przypisuje jej wartość.

```
let a = 1;
```

#### Przypisanie

Przypisuje nową wartość już istniejącej zmiennej bądź polu struktury. Może występować z operacją, która zostanie wykonana na obecnej wartości zmiennej i podawanej wartości.

```
a = 2;
a += 1;
```

#### Wywołanie funkcji

Wywołuje funkcję z podanymi argumentami, tworząc nowy kontekst wywołania i przekazując funkcji kontrolę, dopóki się ona nie zakończy.

```
do_something(1, "string", 1.5);
```

#### `return`

Zwraca z funkcji, z opcjonalną wartością, pozostawiając wszystkie pozostałe instrukcje w funkcji niewykonane.

```
return 1;
```

#### `yield`

Ewaluuje wyrażenie blokowe do wartości, pozostawiając pozostałe instrukcje w danym bloku niewykonane.

```
let a = {
	let b = get_b();
	let c = get_c();
	yield b + c;
}
```

#### `break`

Bezwarunkowo kończy pętlę.

```
break;
```

#### `continue`

Zakańcza obecną iterację pętli, przechodząc do następnej i zapobiegając wykonaniu się instrukcji poniżej.

### Wyrażenia

Wyrażenia są instrukcjami, których wykonanie kończy się wartością.

#### Wyrażenie binarne

Wyrażenie będące dwuargumentową operacją. Do nich należą operacje arytmetyczne, relacyjne oraz dwuargumentowe operacje logiczne.

```
1 + 2 - 3
a > b
1 < 2 & 2 < 3
```

Wyrażenia te mają zdefiniowany priorytet operatorów, odpowiadający priorytetom w matematyce (od najwyższego priorytetu):

* operacje związane z mnożeniem: *, /, %,
* operacje związane z dodawaniem: +, -,
* operacje relacyjne: >, >=, ==, !=, <=, <
* koniunkcja logiczna: &
* alternatywa logiczna: |
* przypisanie

Koniunkcja i alternatywa logiczna ewaluują argumenty tylko do momentu, gdy osiągnięta zostanie wartość, odpowiednio, `false` albo `true`.

#### Wyrażenia unarne

Wyrażenia mające tylko jeden argument. Mają wyższy priorytet niż jakiekolwiek wyrażenie binarne. Są to negacja arytmetyczna i negacja logiczna.

```
-a
!a
```

#### Wyrażenie rzutowania typu: `as`

Rzutuje jeden typ na inny (np. wartość całkowitą na zmiennoprzecinkową). Ma wyższy priorytet niż wyrażenia unarne.

```
1 as float
1.5 as int
```

#### Literał

Stała wartość typu liczbowego, logicznego lub ciągu znaków

```
123
123.456
true
"abcd"
```

#### Identyfikator

Oznacza zmienną i ewaluuje do jej obecnej wartości.

```
abc
```

#### Wywołanie funkcji

Wywołuje funkcję i ewaluuje do jej wartości zwróconej.

```
do_something()
```

#### `if` i `match`

Zostały opisane w sekcji o statementach. Mogą również być wartościami, dlatego zamieszczone są również w tej sekcji.

#### Blok

Blok może ewaluować do wartości za pomocą wyrażenia `yield`. Jest to szczególnie pomocne przy wyrażeniu `match`, kiedy możemy chcieć coś wykonać przed zwróceniem wartości.

```
let a = {
	let b = get_b();
	let c = get_c();
	yield b + c;
}
```

### Przykłady

#### Silnia

```
fn factorial(n) {
    return match n {
        0 -> 1,
        n -> n * factorial(n - 1)
    };
}

fn main() {
	println(factorial(5));
}
```

#### Struktury

```
struct Vector2D {
	float x,
	float y,
}

fn dot(v1, v2) {
	return v1.x * v2.x + v1.y * v2.y;
}

fn main() {
	let v1 = new("Vector2D", 1.0, 2.0);
	let v2 = new("Vector2D", 3.0, 4.0);
	println(dot(v1, v2));
}
```

#### Enumeracje

```
enum MaybeInt {
	Some(int),
	None
}

fn some_if_gt_10(n) {
	if n > 10 {
		return new("MaybeInt", "Some", n);
	} else {
		return new("MaybeInt", "None");
	}
}

fn main() {
	let a = 1;
	match some_if_gt_10(a) {
		MaybeInt.Some(b) -> println(b, " is greater than 10"),
		MaybeInt.None -> println(a, " is less or equal 10")
	}
}
```

## Wykorzystane narzędzia

- Rust 1.61,
- `cargo` — do budowania projektu i wykonywania testów,
- `pre-commit` — do wykonywania akcji sprawdzających formatowanie, błędy kompilacji i testy przed commitem,
- biblioteki:
    - `clap` — do przetwarzania argumentów linii poleceń i wypisywania informacji pomocy,
    - `utf8-read` — do czytania plików znak po znaku,
    - `colored` — do wypisywania kolorowego tekstu do terminala.

## Gramatyka

```
program ::= (function_def | struct_def | enum_def | const_def | comment)*;
comment ::= '//' #'[^\n]';

var_def         ::= 'let' identifier '=' expression ';';
const_def       ::= 'const' identifier '=' expression ';';
enum_def        ::= 'enum' identifier '{' (identifier '(' identifier ')' ',')* (identifier '(' identifier ')')? '}';
struct_def      ::= 'struct' identifier '{' (identifier identifier ',')* (identifier identifier)? '}';
function_def    ::= 'fn' identifier '(' (identifier identifier ',')* (identifier identifier)? ')' block_expr;

statement               ::= var_def
                          | return_stmt
                          | yield_stmt
                          | break_stmt
                          | continue_stmt
                          | for_stmt
                          | while_stmt
                          | if_expr
                          | match_expr
                          | identifier (rest_of_function_call ';' | rest_of_assignment_stmt);
while_stmt              ::= 'while' expression block_expr;
for_stmt                ::= 'for' identifier 'in' range block_expr;
return_stmt             ::= 'return' expression? ';';
yield_stmt              ::= 'yield' expression ';';
break_stmt              ::= 'break' ';';
continue_stmt           ::= 'continue' ';';
rest_of_assignment_stmt ::= identifier assignment_op expression ';';

block_expr  ::= '{' (statement | comment)* '}';
match_expr  ::= 'match' identifier '{' (match_arm ',')* match_arm ','? '}';
if_expr     ::= if_branch else*;
if_branch   ::= 'if' expression block_expr
else        ::= 'else' (if_branch | block_expr)

match_arm    ::= pattern '->' expression;
pattern      ::= pattern_part ('|' pattern_part)*;
pattern_part ::= identifier ('.' identifier ('(' pattern? ')')?)? | literal_range | literal

range ::= value '..' '='? value;
literal_range ::= integer '..' '='? integer;

value                   ::= identifier (rest_of_member_access | rest_of_function_call)? | literal | '(' expression ')';
expression              ::= or_expr | if_expr | match_expr | block_expr;
or_expr                 ::= and_expr (or_op and_expr)*;
and_expr                ::= relational_expr (and_op relational_expr)*;
relational_expr         ::= additive_expr ((relational_op | equality_op) additive_expr)*;
additive_expr           ::= multiplicative_expr (additive_op multiplicative_expr)*;
multiplicative_expr     ::= as_expr (multiplicative_op as_expr)*;
as_expr                 ::= unary_expr ("as" identifier)?;
unary_expr              ::= unary_op* value;
rest_of_member_access   ::= ('.' identifier)+;
rest_of_function_call   ::= '(' (expression ',')* expression? ')';

unary_op            ::= '!' | '-';
or_op               ::= '|';
and_op              ::= '&';
relational_op       ::= '>' | '<' | '>=' | '<=';
equality_op         ::= '==' | '!=';
additive_op         ::= '+' | '-';
multiplicative_op   ::= '*' | '/' | '%';
assignment_op       ::= '=' | '+=' | '-=' | '*=' | '/=' | '%=' | '&&=' | '||=';

literal     ::= number | string;
number      ::= integer | float;
string      ::= '"' character* '"';
float       ::= (integer '.' digit*) | ('.' digit+);
integer     ::= nonzero_digit digit*;
identifier  ::= (letter | '_') (letter | digit | '_')*;

character       ::= #'[^"]';
nonzero_digit   ::= #'[1-9]';
digit           ::= #'[0-9]';
letter          ::= #'[a-zA-Z]';
```

