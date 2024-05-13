#!/usr/bin/awk -f
in_file_field = 0 {
	print
}

{
	# Check for the start of a file field
	if ($0 ~ /file *= */) {
		in_file_field = 1
		# Split the line at the '=', to separate field name from value
		split($0, parts, /= */)
		printf "%s= ", parts[1]
	}
	if (in_file_field == 1) {
		# If we're in a file field, apply the escape_commas function
		# The actual field content is now in $2 due to the FS setting
		split(parts[2], o, /"/)
		split(o[2], file, /, :\//)
		for (i in file) {
			file[i] = escape_commas(file[i])
		}
		printf "\"%s\"%s\n", join(file, 1, length(file), ", :/"), o[3]
	} else {
		# If we're not in a file field, just print the line as it is
		print $0
	}
}


# Included files (-i and/or @include)

# @include "join"	# join.awk --- join an array into a string
#
# Arnold Robbins, arnold@skeeve.com, Public Domain
# May 1993
function escape_commas(field_value)
{
	gsub(",", "\\,", field_value)
	return field_value
}

function join(array, start, end, sep, result, i)
{
	if (sep == "") {
		sep = " "
	} else if (sep == SUBSEP) {
		# magic value
		sep = ""
	}
	result = array[start]
	for (i = start + 1; i <= end; i++) {
		result = result sep array[i]
	}
	return result
}
