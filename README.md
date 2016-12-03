# csg-airbus
Converter to help translate from PDF bus schedules to SQL for the airbus site

For more on updating Airbus data, see the [airbus tutorial](https://github.com/mitchellvitez/UM-CSG-Tutorials/blob/master/airbus.md)

## Usage

Make sure that your data format matches `sample_{east/west}bound.txt`. You can copy most of it from the Airbus PDF schedules.

Running the converter:
```
$ ghc airbus
$ ./airbus eastbound.txt > eastbound.sql
```
