package io.swagger.codegen.utils;

import java.util.ArrayList;
import java.util.List;

import com.google.common.collect.Lists;


public class Combinator {

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
	        System.out.println(combination);
	        combinations.add(combination);
	    }
	    return combinations;
	}
	
	public static class Combination<E> extends ArrayList<E> {
		private static final long serialVersionUID = -5267569151809451656L;
	}
	
}
