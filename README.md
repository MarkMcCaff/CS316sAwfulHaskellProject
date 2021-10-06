# csv-query : CS316 Coursework 2021

This is the repository for the CS316 coursework for 2021.

This coursework is worth 50% of your mark for the course. The other
50% is from the class test in Week 6.

## Description

The goal of this coursework project is to construct a small command
line Haskell program for querying [Comma Separated
Value](https://en.wikipedia.org/wiki/Comma-separated_values) (CSV)
files. For the purposes of this project, we always assume a CSV file
has a header line listing the field names for the rest of the rows.

The basic structure of the project has been written for you. To get a
basic mark you need to fill in the missing implementations. This will
get you an implementation that can read in CSV files, convert them
from rows to records, filter the records by a fixed query, convert
them back, and print them out.

To get a higher mark, you need to implement some automated testing and
extensions to the basic project. See below for details.

## Getting Started

The whole repository is structured as a [stack](FIXME) project in a
Git repository.

Get started by **first** forking the repository on GitLab, and
**then** cloning your fork.

The project is structured into library code in [src](src/) and the
main program in [app/Main.hs](app/Main.hs). There is also space for
tests in [test/Spec.hs](test/Spec.hs). Data files are stored in
[data/](data/).

To build the whole project, use the following command when in the
project directory:

```
$ stack build
```

This will build the project. If this fails, please contact [Robert
Atkey](robert.atkey@strath.ac.uk), or use Mattermost.

To execute the program, use:

```
$ stack exec csv-query data/Countries.csv
```

If you have just started, this will fail with an error reporting that
a function has not yet been implemented. Time to start implementing!

During development, it is often easier to test functions interactively
via `ghci`. To test the functions in a specific module, start `ghci`
like so:

```
$ stack ghci src/Records.hs
```

This will allow you to experiment with the functions in the `Records`
module.

### Running the test suite

The test suite in the skeleton project is empty. To run it, use the
command:

```
$ stack test
```

It should tell you that `"Test suite not yet implemented"`.

## Expected Functionality

Once the unimplemented functions are implemented, the following ought
to work:

```
$ stack exec csv-query data/DoBIH_v17_2_NCH.csv
<list of hills in Scotland with their heights in metres>
```

To change the query, you will need to change the `query` definition in
[app/Main.hs](app/Main.hs). It would be better if the query was not
fixed at compile time. See below for possibly extensions to the basic
system.

## Project Structure

- [app/](app/)
  - [app/Main.hs](app/Main.hs) : contains the source code for the main
    `csv-query` executable.
- [ChangeLog.md](ChangeLog.md) : somewhere for you to record the
  changes you've made, if you wish.
- [data/](data/)
  - [data/Countries.csv](data/Countries.csv) : a data file of country
    codes for the hills database
  - [data/DoBIH_v17_2_NCH.csv](data/DoBIH_v17_2_NCH.csv) : the
    Database of British and Irish Hills (names, countries, and heights
    only)
- [LICENSE](LICENSE) : the License for this software. Fill in your
  name, if you wish.
- [package.yaml](package.yaml) : Description of the various parts of
  the project for Stack. If you want to add additional dependencies,
  then they can be added here.
- [README.md](README.md) : This file
- [src/](src/) : The modules making up the project
  - [src/CSVInput.hs](src/CSVInput.hs) : functions for parsing CSV
    files, you will not need to edit this for the basic project.
  - [src/CSVOutput.hs](src/CSVOutput.hs) : functions for outputing
    values in CSV format. You will need to edit this file to implement
    the basic project.
  - [src/ParserCombinators.hs](src/ParserCombinators.hs) : a library
    for implementing parsers. You will probably not need to edit this
    file. See the Week 8 notes for more information on how parsers
    work.
  - [src/QueryLanguage.hs](src/QueryLanguage.hs) : implementation of a
    simple query language for filtering records. You will need to
    complete the unimplemented functions in this file for the basic
    project.
  - [src/Records.hs](src/Records.hs) : implemention of records (lists
    of fieldname/value pairs) and their conversion to/from rows. You
    will need to fill these in for the basic project.
  - [src/Result.hs](src/Result.hs) : a datatype for results that may
    be either successful or a failure. You will not need to edit this
    file.
- [stack.yaml](stack.yaml) : Configuration file for stack. You will
  probably not need to edit this.
- [test/](test/)
  - [test/Spec.hs](test/Spec.hs) : file to write tests in. You will
    have to fill this in to get credit for the tests.

There will be some generated files when you start working:

- `CS316Coursework2021.cabal` : another description of the project,
  generated from `package.yaml`.
- `stack.yaml.lock` : information for stack on which exact versions of
  the compiler and supporting libraris are being used.
- `.stack-work/` : a folder where the compiled binaries live.

## Marking Scheme

The project is marked out of 50. The marks start being relatively easy
to get, and progress to being harder.

- **10 marks** Completion of the basic missing parts in the given
  implementation. Specifically this is:

   1. The unimplemented functions in [src/Records.hs](src/Records.hs):
      `lookupField`, `rowToRecord`, `rowsToRecords`, `recordToRow`,
      and `recordsToRows`.
   2. The unimplemented functions in
      [src/QueryLanguage.hs](src/QueryLanguage.hs):
      `evalFieldOrLiteral`, `evalCondition`, `evalQuery`, and
      `filterByQuery`.
   3. The unimplemented function in
      [src/CSVOutput.hs](src/CSVOutput.hs): `stringOfRow`.

  Each function has a documentation block above it describing the
  intended functionality.

  To get all ten marks, your code will need to be robust to user
  input, and reasonably well written and concise.

- **5 marks** A test suite. Fill in the file `test/Spec.hs` with tests
  for the functions you have written, so that failing tests are
  reported appropriately. You can either write a list of tests by
  hand, or use a unit testing framework like
  [HUnit](https://hackage.haskell.org/package/HUnit). For more marks,
  explore property based testing using
  [QuickCheck](https://hackage.haskell.org/package/QuickCheck).

  You should document your test suite to say what is being tested by
  each test.

  For advice on how to use these testing frameworks, please ask on
  Mattermost.

- **35 marks** Additional features. Suggested additional features:

  - Letting the user specify the query on the command line. You will
    need to write a parser for queries, and work out how to read them
    from the command line. For robustness, it would be advisable to
  - Allowing joining of multiple CSV files on common fields. For
    example, the [data/DoBIH_v17_2.csv](data/DoBIH_v17_2.csv) file
    uses codes for the counties which are listed in
    [data/Countries.csv](data/Countries.csv). If the user could join
    these files on the country, then the output would be more human
    readable.
  - Customisation of the output. Possibilities:
    - Subsets of the fields in the original CSV file
    - Computed fields (e.g., the height in metres converted to feet)
    - Alternative output formats (Tab Separated Values, HTML, Excel
      spreadsheets, ...)

  You will marked on:
  - Documentation of the additional feature
  - The complexity of the implementation
  - The robustness and good coding style of the additional feature

## Submission

Make sure that your final submission is pushed to your GitLab
repository.

This is a

Submit a small **text** file containing a link to your repository on
GitLab and any additional information you want to add to describe what
you have done. You should also include the following statement: *The
code in my GitLab repository is entirely my own work, except where it
is from the original project skeleton or has been clearly stated. No
code has been copied from classmates. I understand this code will be
checked with plagiarism checkers. I give permission for the source
code to be inspected on CIS servers.*

Standard late submission and illness rules apply. If you are ill you
must certify on Pegasus and request an extension before the deadline
using a MyPlace extension request.

## Credits

The database of hills in [data/DoBIH_v17_2.csv](data/DoBIH_v17_2.csv)
is a copy of the [Database of British and Irish
Hills](http://www.hills-database.co.uk/downloads.html), v17.2, CSV
version. The original file is very large, so I have limited it to just
the names, countries, and height in metres.
