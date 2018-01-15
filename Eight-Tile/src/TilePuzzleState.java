import java.util.*;


public class TilePuzzleState extends State {
	/* Tile puzzle state is a configuration of the board. 
	 * For simplicity, the board must be square (NxN), with one
	 * tile missing - allowing for 2, 3, or 4 moves. 
	 * 
	 * By default, the board is 3x3 - as is the classic "8-tile" game. 
	 */
	
	// number of tiles on each side 
	private int dimension; 
	/* State of the board is implemented as an integer array, where
	* the "empty" tile is represented as the integer zero */
	private int [] board;
	// the depth in the search tree the state is
	private int depth;
	
	
	// Most general constructor
	public TilePuzzleState(int dimension, int [] board, int depth){
		
		boolean contains_zero = false;
		for(int i : board){
			if(i == 0){
				contains_zero = true;
			}
		}
		if(contains_zero){
			this.dimension = dimension;
			this.board = board;
			this.depth = depth;
		}
		else{
			throw new IllegalArgumentException ("One element in input array must be "
					+ "`0` to symbolize the empty tile.");	
		}	
	}
	
	// if initialize without depth - assume depth is toplevel
	public TilePuzzleState(int dimension, int [] board){
		this(dimension, board, 0);
	}

	// If only specify a dimension, create a random board of that dimension
	public TilePuzzleState(int dimension){
		// initialize a list so that you can randomly 
		// permute the generated board (int array)
		ArrayList <Integer> tmpList = new ArrayList<Integer>();
		for (int i = 0; i < Math.pow(dimension, 2); i++){
			tmpList.add(i);
		}
		// shuffle the temporary List
		Collections.shuffle(tmpList);
		// convert this list to an array, an store as board attribute
		int [] tmpArray = new int [dimension*dimension];
		for (int i = 0; i < tmpList.size(); i++){
			tmpArray[i] = tmpList.get(i);
		}
		
		this.dimension = dimension;
		this.board = tmpArray;
		this.depth = 0;
	}
	// If specify nothing, create a random 3x3 board
	public TilePuzzleState(){
		// be default, create a 3x3 board
		this(3);
	}
	// If specify just the board: 
	public TilePuzzleState(int [] board){
		// check if inputed board length if perfect square
		if (Math.pow((int) Math.sqrt(board.length), 2) != board.length){
			throw new IllegalArgumentException("Input board must be square.");
		}
		
		boolean contains_zero = false;
		for(int i : board){
			if(i == 0){
				contains_zero = true;
			}
		}
		if(contains_zero){
			this.dimension = (int) Math.sqrt(board.length);
			this.board = board;
			this.depth = 0;
		}
		else{
			throw new IllegalArgumentException ("One element in input array must be "
					+ "`0` to symbolize the empty tile.");	
		}
		
	}
	
	// get the current state of the board (as an integer array)
	public int [] get_board(){
		return this.board;
	}
	
	// get the current dimension
	public int get_dimension(){
		return this.dimension;
	}
	
	// get the current depth
	public int get_depth(){
		return this.depth;
	}
	
	// find the index of the empty tile
	public int emptyTile(){
		for (int i = 0; i < this.board.length; i++){
			if(this.board[i] == 0){
				return i;
			}
		}
		throw new IllegalArgumentException ("One element in input array must be "
				+ "`0` to symbolize the empty tile."); 
	}
	
	// print the current state of the board
	@Override
	void print() {
		// create a printing function for an initialized State
		int nrow = this.dimension;
		int ncol = nrow;
		System.out.println();
		for (int i = 0; i < nrow ; i++){
			for (int j = 0; j < ncol; j++){
				// print the left boundary, then the number
				System.out.print("|");
				if (this.board[i*ncol + j] != 0){
					System.out.print(this.board[i*ncol + j]);
				}
				else{
					System.out.print(" ");
				}
			}
			
			// close row
			System.out.println("|"); 
		}
	}
	
	
	// check for equality between states
	public boolean equals(TilePuzzleState s) {
		return (this.board.equals(s.get_board()));
	}

}
