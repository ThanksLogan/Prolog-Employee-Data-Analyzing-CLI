:- use_module(library(csv)).

% Predicate to read data from a CSV file and store it as rules
read_csv_and_store :-
    % Provide the full path to your CSV file
    % Desktop path for csv file: Directory = '/mnt/c/CS420/Assignment4/'
    % Mac path for csv file: Directory = '/Users/loganforeman/cs420/Prolog/'
    Directory = '/mnt/c/CS420/Assignment4/',
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
	Salary,	Bonus, Country, City, Exit_Date)) :-
    assert(employee(EEID, Full_Name, Job_Title, Department, Business_Unit, Gender, Ethnicity, Age, Hire_Date, Salary, Bonus, Country, City, Exit_Date)).

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

% 2.10: 
% HELPER FUNCTION (REMOVES $ and , AND CONVERTS SALARY STRING TO NUM):

is_highly_paid_senior_manager(Name, Salary) :-
    employee(_, Name, Job_Title, _, _, _, _, _, _, Salary, _, _, _, _),
    atom_concat('Sr. ', _, Job_Title),  % Check if the job title starts with "Sr."
    convert_salary_to_number(Salary, SalaryNumber),
    SalaryNumber > 120000,
    Salary = SalaryNumber.
