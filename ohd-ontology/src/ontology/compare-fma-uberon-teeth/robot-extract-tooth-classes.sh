robot extract --method MIREOT \
		--input ../imports/fma-jaws-teeth.owl \
		--branch-from-term "obo:FMA_12516" \
		--output fma-teeth.owl

robot query -i fma-teeth.owl -q fma-tooth-classes.rq out-fma-tooth.tmp.tsv

cat out-fma-tooth.tmp.tsv
