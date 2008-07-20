#!/bin/sh

echo "Preprocessing..." 
ruby preprocess.rb > tmp1.pl

echo "Processing..."
prism -g process.pl
#rm -f tmp1.pl

echo "Post processing..."
ruby postprocess.rb > tmp3.pl
#rm -f tmp2.pl

# Test FOC compilability
cat > remove_errors.pl << EOD
:- ['tmp3.pl'].
:- current_output(Cur), open('focable.pl',write,Stream), set_output(Stream), listing, close(Stream), set_output(Cur).
EOD

prism -g remove_errors.pl &> /dev/null
rm -f remove_errors.pl
echo "done - Look at focable.pl"
