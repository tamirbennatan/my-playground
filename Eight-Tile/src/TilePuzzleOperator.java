public class TilePuzzleOperator extends Operator {
	private  int empty_index; 
	private  int filled_index;
	
	public TilePuzzleOperator(int empty, int filled){
		this.empty_index = empty; 
		this.filled_index = filled;
	}
	
	public TilePuzzleState applyOp(TilePuzzleState s){
		int [] current_board = s.get_board(); 
		int [] new_board = new int [current_board.length];
		System.arraycopy(current_board, 0, new_board, 0, current_board.length);
		int current_dim = s.get_dimension();
		
		// swap the location of the empty tile
		new_board[this.empty_index] = new_board[this.filled_index];
		new_board[this.filled_index] = 0; 
		
		return(new TilePuzzleState(current_dim, new_board));
	}
}
