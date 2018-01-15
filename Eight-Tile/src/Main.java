import java.util.*;

public class Main {

	public static void main(String[] args) {
		// Initialize a state
		TilePuzzleState s = new TilePuzzleState();
		
		System.out.println("Initial state");
		s.print();
		System.out.println();
		
		// make a problem
		TilePuzzleProblem start_problem = new TilePuzzleProblem(s);
		
		// get the next legal ops
		Vector<TilePuzzleOperator> first_ops = start_problem.getLegalOps(s);

		
		for (TilePuzzleOperator op : first_ops){
			TilePuzzleState newstate = op.applyOp(s);
			newstate.print();
		}
	}

}
