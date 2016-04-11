#!/usr/bin/awk -f

##
##Parser inline for data reduction. Convert the raw style data in CSV format.  
##Usage: ./convert_cvs.awk inputfilename.dat > outputfilename.dat
##Date: 6-3-2016


BEGIN{	
	print "#x,y,z,t";
}

{
	if( $1 == "M:x" ){
		print $6/100.","$3/100.","$9/100.","$12/100.
	}else{
	}
}
END{

}



