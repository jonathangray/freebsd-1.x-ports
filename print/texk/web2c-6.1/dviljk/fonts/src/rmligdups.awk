# At kern steps, just save away the value, so only the last one will be output.
/\(KRN/		{ lk_table[$3] = $0; next }

# Assume (STOP)'s are in the right place.
# This rearranges the order, but oh well.
# It's random by the time the vpl file has been written anyway.
/\(STOP\)/	{ for (lk in lk_table)
		    {
		      print lk_table[lk];
		      delete lk_table[lk];
		    }
		}

		{ print }
