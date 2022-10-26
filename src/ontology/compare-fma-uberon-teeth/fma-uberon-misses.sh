miss_count=0  # keep track of misses and matches
match_count=0

for fma_id in $(rev fma-tooth-classes.tsv | cut -f1 -d$'\t' | rev)
do
  count=$(grep -c $fma_id uberon-fma-results.tsv)
  if [ $count -gt 0 ]
  then
     ((match_count++))
  else
     ((miss_count++))
	 label=$(grep $fma_id fma-tooth-classes.tsv | cut -f2 -d$'\t')
	 echo "$fma_id\t$label"
  fi
done

# used for checking results
#echo "matches: $match_count"
#echo "misses: $miss_count"
