import java.util.Vector;

// An abstract class for a search problem 
public abstract class Problem<T,U> {
	// Keep track of start state
	T startState;
	// A method for determining if a current method is the goal
	abstract boolean isGoal(T s);
	// Return a vector of legal operations one may carry out
	abstract Vector<U> getLegalOps(T s);
	// Get the next state after performing an operation
	abstract State nextState(T s, U op);
	// Get the cumulative cost that will result from applying an operator
	abstract float cost(T s, U op); 
	
	// return the start state of the problem
	public T getStartState(){
		return this.startState;
	}
	
}
