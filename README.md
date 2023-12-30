# Prolog Assignment: Employee Data Analysis

## Overview

See project report for photos.

This project involves a Prolog program designed to analyze employee data from a CSV file. The program translates the CSV data into Prolog knowledge base as FACTS, implements specific rules for data queries, and allows testing with various queries.

## Getting Started

### Prerequisites
+ SWI-Prolog
+ Access to EDORAS server for file transfer

### Installation

+ Clone the repository or download the source code.
+ Ensure EmployeeData.csv is located in the same directory as the Prolog file.

### Running the Program

+ Open SWI-Prolog.
+ Load the program: ?- [Assign4].
+ Execute desired queries as per the examples provided.

***

## Code Description

**Assign4.pl**
CSV to FACTS Conversion: Contains code to read EmployeeData.csv and convert each row into a Prolog fact.
Rule Definitions:
is_seattle_employee(Name): Determines if an employee works in Seattle.
is_senior_manager_in_IT(Name): Identifies if an employee is a senior manager in the IT department.

**EmployeeData.csv**
The CSV file with employee data used as the input for the Prolog program.
Testing
The program has been thoroughly tested with various scenarios.
Example queries and their expected outputs are provided for reference.

## Acknowledgments

Special thanks to Professor Krishna Ramamoorthy for guidance.

## Author

ThanksLogan
