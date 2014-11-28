object Nodes {
    def main() : Unit = {
        if(new NodePresenter().test()) { println("Ok"); } else { println("error"); }
    }
}

class NodePresenter {
    def test() : Bool = {
        var tree: Node;
        tree = new Branch().init("A", new Leaf().init("B"), new Leaf().init("C")).visit();
        return true;
    }
}

class Node {

    def visit(): Node = {
        println("Node");
        return this;
    }

}

class Branch extends Node {
    var name: String;
    var right: Node;
    var left: Node;
    
    def init(n : String, r: Node, l: Node) : Node = {
        name = n;
        right = r;
        left = l;

        return this;
    }

    def visit(): Node = {
        var tmp: Node;
        println(">> " + name + " >>");
        tmp = right.visit();
        tmp = left.visit();
        println("<< " + name + " <<");
        return this;
    }
    
}

class Leaf extends Node {
    var name: String;
    
    def init(n : String) : Node = {
        name = n;

        return this;
    }

    def visit(): Node = {
        println("Left " + name);
        return this;
    }
}