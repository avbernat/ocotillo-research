AICprobs <- function ( x ) {

	y <- x - min ( x )

	tmp <- exp ( -1/2 * y )

	return ( tmp / sum ( tmp ) )



}