package io.swagger.codegen.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.List;

import org.junit.Test;

import com.google.common.base.Joiner;
import com.google.common.collect.Lists;

import io.swagger.codegen.utils.Combinator.Combination;

public class CombinatorTest {

	@Test
	public void testGetCombinations_0elements() {
		List<String> list = Lists.newArrayList();
		
		List<Combination<String>> combinations = Combinator.getCombinations(list);
		
		for (int i=0; i<combinations.size(); i++) {
			Combination<String> combination = combinations.get(i);
			switch (i) {
			case 0: 
				assertCombination(combination, "");
				break;
			case 1:
				fail("Unexpected case; we do not anticipate that many combinations.");
			}
		}
	}
	
	@Test
	public void testGetCombinations_1element() {
		List<String> list = Lists.newArrayList("A");
		
		List<Combination<String>> combinations = Combinator.getCombinations(list);
		
		for (int i=0; i<combinations.size(); i++) {
			Combination<String> combination = combinations.get(i);
			switch (i) {
			case 0: 
				assertCombination(combination, "");
				break;
			case 1: 
				assertCombination(combination, "A");
				break;
			case 2: 
				fail("Unexpected case; we do not anticipate that many combinations.");
			}
		}
	}
	
	@Test
	public void testGetCombinations_2elements() {
		List<String> list = Lists.newArrayList("A", "B");
		
		List<Combination<String>> combinations = Combinator.getCombinations(list);
		
		for (int i=0; i<combinations.size(); i++) {
			Combination<String> combination = combinations.get(i);
			switch (i) {
			case 0: 
				assertCombination(combination, "");
				break;
			case 1: 
				assertCombination(combination, "A");
				break;
			case 2: 
				assertCombination(combination, "B");
				break;
			case 3: 
				assertCombination(combination, "A,B");
				break;
			case 4: 
				fail("Unexpected case; we do not anticipate that many combinations.");
			}
		}
	}
	
	@Test
	public void testGetCombinations_3elements() {
		List<String> list = Lists.newArrayList("A", "B", "C");
		
		List<Combination<String>> combinations = Combinator.getCombinations(list);
		
		for (int i=0; i<combinations.size(); i++) {
			Combination<String> combination = combinations.get(i);
			switch (i) {
			case 0: 
				assertCombination(combination, "");
				break;
			case 1: 
				assertCombination(combination, "A");
				break;
			case 2: 
				assertCombination(combination, "B");
				break;
			case 3: 
				assertCombination(combination, "A,B");
				break;
			case 4: 
				assertCombination(combination, "C");
				break;
			case 5: 
				assertCombination(combination, "A,C");
				break;
			case 6: 
				assertCombination(combination, "B,C");
				break;
			case 7: 
				assertCombination(combination, "A,B,C");
				break;
			case 8: 
				fail("Unexpected case; we do not anticipate that many combinations.");
			}
		}
	}
	
	/**
	 * This test should be seen in relation to {@link CombinatorTest#testGetCombinations_3elements()}.
	 * We assert that the order of the elements of the provided list matters.
	 */
	@Test
	public void testGetCombinations_3elements_rearranged() {
		List<String> list = Lists.newArrayList("A", "C", "B");
		
		List<Combination<String>> combinations = Combinator.getCombinations(list);
		
		for (int i=0; i<combinations.size(); i++) {
			Combination<String> combination = combinations.get(i);
			switch (i) {
			case 0: 
				assertCombination(combination, "");
				break;
			case 1: 
				assertCombination(combination, "A");
				break;
			case 2: 
				assertCombination(combination, "C");
				break;
			case 3: 
				assertCombination(combination, "A,C");
				break;
			case 4: 
				assertCombination(combination, "B");
				break;
			case 5: 
				assertCombination(combination, "A,B");
				break;
			case 6: 
				assertCombination(combination, "C,B");
				break;
			case 7: 
				assertCombination(combination, "A,C,B");
				break;
			case 8: 
				fail("Unexpected case; we do not anticipate that many combinations.");
			}
		}
	}
	
	private void assertCombination(Combination<String> combination, String expectation){
		assertEquals(expectation, Joiner.on(",").join(combination));
	}

}
