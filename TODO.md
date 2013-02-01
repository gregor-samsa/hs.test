
# short term
- import files 
  + exif jpg files
+ query by tag

# Misc
+ Add a filter to existing example - eg. `where` Name eq "Richard"

# Features
+ Add tags + query by 
+ Files with Exif
  + Read real exif (libexif)
+ Query by date range
+ Make a film roll

# Build
+ Add some basic tests

# Done
- do a traversable example
- do it with a gadt
- Cabal project
- Proplist in the gadt
- Records
    - just use a simple gadt for now

# Maybe future
      x How to project a record using a String eg.
      x project "field1" rec
        x project :: String -> (FieldType -> FieldType) -> Rec -> Maybe FieldType
  x use pipes
