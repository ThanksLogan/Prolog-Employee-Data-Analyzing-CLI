:- use_module(library(csv)).

% Predicate to read data from a CSV file and store it as rules
read_csv_and_store :-
    % Provide the full path to your CSV file
    Directory = '/Users/loganforeman/cs420/Prolog/',
    Filename = 'EmployeeData.csv',
    atomic_concat(Directory, Filename, Path),
    csv_read_file(Path, Rows, []),
    process_rows(Rows).

% Process each row in the CSV file and store data as rules
process_rows([]).
process_rows([Row|Rows]) :-
    process_row(Row),
    process_rows(Rows).

% Store data from a row as a rule
process_row(row(EEID, Full_Name, Job_Title,	Department,
	Business_Unit,	Gender,	Ethnicity, Age,	Hire_Date,
	Annual_Salary,	Bonus, Country, City, Exit_Date)) :-
    assert(employee(EEID, Full_Name, Job_Title, Department, Business_Unit, Gender, Ethnicity, Age, Hire_Date, Annual_Salary, Bonus, Country, City, Exit_Date)).

% 2.1: method to check if employee is from certain city 
is_seattle_employee(Name) :-
	employee(_,Name,_,_,_,_,_,_,_,_,_,_,'Seattle',_).

% 2.2:
is_senior_manager_in_IT(Name) :-
	employee(_,Name,'Sr. Manger','IT',_,_,_,_,_,_,_,_,_,_).

% 2.3:
is_director_finance_miami(Name) :-
	employee(_,Name,'Director','Finance',_,_,_,_,_,_,_,_,'Miami',_).

% 2.4:
is_asian_US_manufacturing_40M(Name, Business_Unit, Gender, Ethnicity, Age) :-
    employee(_, Name, _, _, Business_Unit, Gender, Ethnicity, Age, _, _, _, 'United States', _, _),
    Business_Unit = 'Manufacturing',
    Gender = 'Male',
    Ethnicity = 'Asian',
    Age > 40.

% 2.5:
greet(EEID) :-
    employee(EEID, Name, Job_Title, Department, Business_Unit, _, _, _, _, _, _, _, _, _),
    format('Hello, ~w, ~w of ~w, ~w!', [Name, Job_Title, Department, Business_Unit]).

% retract(employee('EEID', _, _, _, _, _, _, _, _, _, _, _, _, _)). ')

% 2.6:
% figure out time for Years_to_retire using 'is' operator
years_until_retirement(Name, Age,Years_to_retire) :-
    employee(_, Name, _, _, _, _, _, Age, _, _, _, _, _, _),
    Years_to_retire is 65 - Age.

% 2.7:
is_rd_black_midAge(Name, Business_Unit, Ethnicity, Age) :-
    employee(_, Name, _, _,Business_Unit, _, Ethnicity, Age, _, _, _, _, _, _),
    Business_Unit = 'Research & Development',
    Ethnicity = 'Black',
    Age > 24,
    Age < 51.

% 2.8:
% Uses or operator semicolon and needs parenthesis
is_ITorFin_PHXorMIAorAUS(Name, Department,City) :-
    employee(_,Name,_,Department,_,_,_,_,_,_,_,_,City,_),
    (Department = 'IT'; Department = 'Finance'),
    (City = 'Phoenix'; City = 'Miami'; City = 'Austin').

% 2.9:
% matches query atoms "Sr. " with atoms in JobTitle.
is_female_senior_role(Name, Job_Title) :-
    employee(_, Name, Job_Title, _, _, 'Female', _, _, _, _, _, _, _, _),
    atom_concat('Sr. ', _, Job_Title). 

% 2.10
% Helper predicate to check if a character should be kept
keep_char(Char) :-
    Char \= '$',
    Char \= ',',
    Char \= ' '.

% Predicate to remove certain characters from an atom
remove_chars(OriginalAtom, ModifiedAtom) :-
    atom_chars(OriginalAtom, CharList),
    include(keep_char, CharList, FilteredCharList),
    atom_chars(ModifiedAtom, FilteredCharList).

is_highly_paid_senior_manager(Name, Annual_Salary) :-
    employee(_,Name,'Sr. Manger',_,_,_,_,_,_,Annual_Salary,_,_,_,_),
    remove_chars(Annual_Salary, ModSalary),
    atom_number(ModSalary, SalaryNumber),
    SalaryNumber > 120000.


% 2.11 
is_divisible(X,Y) :-
    0 is X mod Y, !.

is_divisible(X,Y) :- 
 	X > Y+1, is_divisible(X, Y+1).

is_prime(2) :- true, !.
is_prime(X) :- X < 2, !, false.
is_prime(X) :-
	not(is_divisible(X, 2)).

is_prime_age(Name, Age) :-
	employee(_, Name, _, _, _, _, _, Age, _, _, _, _, _, _),
	is_prime(Age).

% 2.12
% gets job title, then goes through entire list accumulating total sum and Count
salary_for_job_title(Job_Title, SalaryNumber) :-
    employee(_, _, Job_Title, _, _, _, _, _, _, Annual_Salary, _, _, _, _),
    remove_chars(Annual_Salary, ModSalary),
    atom_number(ModSalary, SalaryNumber),
    SalaryNumber > 1.

average_salary(Job_Title, AverageSalary) :-
    findall(SalaryNumber, salary_for_job_title(Job_Title, SalaryNumber), Salaries),
    sum_list(Salaries, TotalSalary),
    length(Salaries, Count),
    Count > 0,
    AverageSalary is TotalSalary / Count.


% 2.13
keep_char_two(Char) :-
    Char \= '%',
    Char \= ' '.

% Predicate to remove certain characters from an atom
remove_chars_two(OriginalAtom, ModifiedAtom) :-
    atom_chars(OriginalAtom, CharList),
    include(keep_char_two, CharList, FilteredCharList),
    atom_chars(ModifiedAtom, FilteredCharList).

total_salary(Name, AverageSalary) :-
 	employee(_, Name, _, _, _, _, _, _, _, Annual_Salary, Bonus, _, _, _),
    remove_chars(Annual_Salary, ModSalary),
    remove_chars_two(Bonus, ModBonus),
    atom_number(ModSalary, SalaryNumber),
    atom_number(ModBonus, BonusNumber),
    AverageSalary is SalaryNumber + (SalaryNumber * (BonusNumber / 100)).

% 2.14
takehome_salary(Name, Job_Title, Take_home_salary) :-
    employee(_, Name, Job_Title, _, _, _, _, _, _, Annual_Salary, _, _, _, _),
    total_salary(Name, TotalSalary),
    tax_percentage(TotalSalary, TaxPercent),
    TaxAmount is TotalSalary * (TaxPercent / 100),
    Take_home_salary is TotalSalary - TaxAmount.

% Helper predicate to determine tax percentage based on salary range
tax_percentage(Salary, Tax) :-
    (   Salary < 50000 -> Tax = 20
    ;   Salary < 100001 -> Tax = 25
    ;   Salary < 200001 -> Tax = 30
    ;   Tax = 35 ). % <- else branch basically 
	
% 2.15
total_years(Name, Hire_Date,Exit_Date) :-
    employee(_, Name,_, _, _, _, _, _, Hire_Date, _, _, _, _, Exit_Date).

	

                                                              














