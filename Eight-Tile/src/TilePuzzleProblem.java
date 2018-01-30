import java.util.Vector;

public class TilePuzzleProblem extends Problem<TilePuzzleState, TilePuzzleOperator> {
	
	private int [] goal; 
	State startState;
	
	// If initialized with an input board
	TilePuzzleProblem(TilePuzzleState startState){
		this.startState = startState;
		this.goal = makeGoal(startState.get_dimension());
	}
	// if initialized with an input dimension
	TilePuzzleProblem(int dimension){
		// set the goal
		int [] tmpGoal = makeGoal(dimension);
		
		// store the goal
		this.goal = tmpGoal;
		this.startState = new TilePuzzleState(dimension);
	}
	// by default, initialize with a random 3x3 board. 
	TilePuzzleProblem(){
		this(3);
	}
	
	// given the dimensions of a board, make an array representing the goal states
	public static int [] makeGoal(int dimension){
		int [] tmpGoal = new int [dimension*dimension]; 
		for (int i = 0; i < tmpGoal.length; i++){
			tmpGoal[i] = i+1;
		}
		// set the last one to be zero
		tmpGoal[tmpGoal.length-1] = 0;
		
		return tmpGoal;
	}

	@Override
	boolean isGoal(TilePuzzleState s) {
		// get current state of the board
		TilePuzzleState goal_state = new TilePuzzleState(this.goal);
		return(s.equals(goal_state));
	}

	@Override
	Vector<TilePuzzleOperator> getLegalOps(TilePuzzleState s) {
		// find the index of the empty tile
		int empty_index = s.emptyTile();
		// set the bounds for the minimum and maximum locations in the board
		int min_index = 0;
		int max_index = (int)Math.pow(s.get_dimension(),2) - 1;
		
		// store the problem dimension, for future reference
		int dimension = s.get_dimension();
		
		// Accumulate legal ops in a vector
		Vector<TilePuzzleOperator> legalOps = new Vector<TilePuzzleOperator>();
		
		// Can you switch with the tile to the left? 
		if(empty_index - 1 >= min_index & (empty_index % dimension) != 0){
			legalOps.addElement(new TilePuzzleOperator(empty_index, empty_index - 1));
		}
		
		// Can you switch with the tile to the right?
		if(empty_index + 1 <= max_index & ((empty_index + 1) % dimension) != 0){
			legalOps.addElement(new TilePuzzleOperator(empty_index, empty_index + 1));
		}
		
		// Can you switch with the tile above?
		if(empty_index - dimension >= min_index ){
			legalOps.addElement(new TilePuzzleOperator(empty_index, empty_index - dimension));
		}
		
		// Can you switch with the tile below?
		if(empty_index + dimension <= max_index ){
			legalOps.addElement(new TilePuzzleOperator(empty_index, empty_index + dimension));
		}
		
		return legalOps;
	}

	@Override
	State nextState(TilePuzzleState s, TilePuzzleOperator op) {
		return op.applyOp(s);
	}

	@Override
	float cost(TilePuzzleState s, TilePuzzleOperator op) {
		// cost of performing an operation
		return 1;
	}
	
	// return the goal
	public int [] getGoal(){
		return this.goal;
	}
	
}
