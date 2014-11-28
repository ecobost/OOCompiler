
object BinaryTree{

	def main(): Unit = {	
 		println(new BTComputer().compute());
	

	}
}

class BTComputer {

	var theTree : BT;
	var temp : Bool;

	def compute(): String ={
				
		theTree = new BT();
		temp = theTree.setUpBT();
		temp = theTree.addNode(50, "Boss");
		temp = theTree.addNode(25, "Vice President");
		temp = theTree.addNode(15, "Office Manager");
		temp = theTree.addNode(30, "Secretary");
		temp = theTree.addNode(75, "Sales Manager");
		temp = theTree.addNode(85, "Salesman 1");

		// Different ways to traverse binary trees
		temp = theTree.inOrderTraverseTree(theTree.getRoot());
		// theTree.preorderTraverseTree(theTree.getRoot());
		// theTree.postOrderTraverseTree(theTree.getRoot());
		// Find the node with key 75

		println("Node with the key 75");

		println(theTree.findNode(75).toString());
		
		return " *OK* ";

	}

}


class Null extends Node {}

class  BT {
	var root : Node;
	var null : Null;
	var temp : Bool;
	
	
	
	
	def getRoot() : Node = {
	
	return root;
	}
	
	def setUpBT() : Bool ={
	null = new Null();
	root = null;
	temp = false;
	return true;
	
	}


	def addNode(key : Int, name : String) :  Bool  =	{
		// Create a new Node and initialize it
		var newNode : Node; 
		var parent : Node;
		var focusNode : Node;
		var stop : Bool;
		
		stop = true;
			

		newNode = new Node();
		
        temp = newNode.setNode(key, name, null);
		// If there is no root this becomes root
		if (root == null) {
		root = newNode;
		
		}
		else{
		// Set root as the Node we will start								
		// with as we traverse the tree	
		focusNode = root;			
						
											
		// Future parent for our new Node
		
		while (stop) {
		// root is the top parent so we start								
		// there												
		parent = focusNode;
		// Check if the new node should go on
		// the left side of the parent node
		

		if (key < focusNode.getKey()) {

		// Switch focus to the left child
		

		focusNode = focusNode.getLeftChild();

		// If the left child has no children

		if (focusNode == null) {

		// then place the new node on the left of it

		temp = parent.setLeftChild(newNode);
		
		stop = false;
		

		}

		} else { 
		// If we get here put the node on the right

		focusNode = focusNode.getRightChild();

		// If the right child has no children

		if (focusNode == null) {

		// then place the new node on the right of it

		temp = parent.setRightChild(newNode);
		stop = false;

					}
				  }

				}

			}
			
			
		return true;
	}

	// All nodes are visited in ascending order
	// Recursion is used to go to one node and
	// then go to its child nodes and so forth

	def inOrderTraverseTree(focusNode : Node): Bool = {

		if ( !(focusNode == null)) {

			// Traverse the left node

			temp = this.inOrderTraverseTree(focusNode.getLeftChild());

			// Visit the currently focused on node

			println(focusNode.toString());

			// Traverse the right node

			temp = this.inOrderTraverseTree(focusNode.getRightChild());

		}
		return true;
	}

	def preorderTraverseTree(focusNode : Node): Bool = {

		if (!(focusNode == null)) {

			println(focusNode.toString());

			temp = this.preorderTraverseTree(focusNode.getLeftChild());
			temp = this.preorderTraverseTree(focusNode.getRightChild());

		}
		return true;
	}

	def postOrderTraverseTree(focusNode : Node): Bool = {

		if (!(focusNode == null)) {

		temp = this.postOrderTraverseTree(focusNode.getLeftChild());
		temp = this.postOrderTraverseTree(focusNode.getRightChild());

			println(focusNode.toString());

		}
		return true;
	}

	def findNode(key : Int) : Node = {

		// Start at the top of the tree

		var focusNode : Node;
		var ret : Node;
		focusNode = root;

		// While we haven't found the Node
		// keep looking

		while (!(focusNode.getKey() == key)) {

			// If we should search to the left

			if (key < focusNode.getKey()) {

				// Shift the focus Node to the left child

				focusNode = focusNode.getLeftChild();

			} else {

				// Shift the focus Node to the right child

				focusNode = focusNode.getRightChild();

			}

			// The node wasn't found

			if (focusNode == null)
				ret = null;

		}
		ret = focusNode;
		return ret;

	}

}

class Node {

	var null : Null;
	var key : Int;
	var name : String;
	var leftChild : Node;
	var rightChild : Node;
		
	
	

	def setNode(key1 : Int, name1 : String, null1 : Null) : Bool = {

		key = key1;
		name = name1;
		null = null1;
		leftChild =null1;
		rightChild =null1;
		return true;
		
	}
	
	def getKey(): Int ={

	return key;

	}

	def getLeftChild(): Node={

	return leftChild;

	}
	def getRightChild(): Node ={

	return rightChild;

	}
	def setLeftChild(child : Node): Bool={

	leftChild = child;
  	return true;
	}
	def setRightChild(child : Node): Bool ={

	rightChild = child;
	return true;
	}

	def toString(): String ={
        var resultat : String;
		resultat = name + " has the key " + key	;

		return resultat;

		/*
		 * return name + " has the key " + key + "\nLeft Child: " + leftChild +
		 * "\nRight Child: " + rightChild + "\n";
		 */

	}

}
