package io.swagger.codegen.utils;

import java.util.ArrayList;
import java.util.List;

import com.google.common.collect.Lists;


/**
 * Note that the intended usage of this utility class is to construct combinations of operation parameters with
 * the goal of creating overloaded methods. As such: If the swagger doc adds an optional parameter to the end
 * of the list of parameters for a given operation, the new combinations that arise from this
 * will appear *in the end* of the list of combinations created from the combinator.
 *  
 * @author M83522
 *
 */
public class Combinator {

	public static class Combination<E> extends ArrayList<E> {
		private static final long serialVersionUID = -5267569151809451656L;
	}
	
	
	/**
	 * Below method is a slightly modified version of the iterative approach presented on this blog:
	 * <br>
	 * <a href="https://theproductiveprogrammer.blog/GeneratingCombinations.java.php">https://theproductiveprogrammer.blog/GeneratingCombinations.java.php</a>
	 * 
	 * @param elems a list of the elements that should be combined
	 * @return a list of {@link Combination}s (See <a href="https://en.wikipedia.org/wiki/Combination">https://en.wikipedia.org/wiki/Combination</a>)
	 */
	public static <E> List<Combination<E>> getCombinations(List<E> elems) {
		List<Combination<E>> combinations = Lists.newArrayList();
	    int n = elems.size();
	    for(int num = 0;num < (1 << n);num++) {
	    	Combination<E> combination = new Combination<>();
	        for(int ndx = 0;ndx < n;ndx++) {
	            // (is the bit "on" in this number?)
	            if((num & (1 << ndx)) != 0) {
	                // then it's included in the list
	                combination.add(elems.get(ndx));
	            }
	        }
	        combinations.add(combination);
	    }
	    return combinations;
	}
	
	
}
