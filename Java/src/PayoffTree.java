import java.util.Stack;

public class PayoffTree {
    public static int n = 3; // number of periods
    public static double S0 = 100; // price of the underlying at t=0
    public static double sigma = 0.5; // measure of standard deviation of the underlying
    // Up and Down movements
    public static double U = Math.pow(Math.E, sigma / Math.sqrt(n));
    public static double D = 1 / U;

    private TreeNode root; // initial node

    public PayoffTree() {
        root = new TreeNode(S0, 0, null);
    }
    


    public double getValue() {
        double value = 0;
        int traversed = 0;

        // Stack for processing the nodes of the binary tree
        Stack<TreeNode> nodeStack = new Stack<TreeNode>();
        nodeStack.push(root);

        while (!nodeStack.empty()) {

            // Pop the top item from stack
            TreeNode currNode = nodeStack.peek();
            nodeStack.pop();

            // Push right and left children of the popped node to stack
            if (currNode.getUpNode() != null) {
                nodeStack.push(currNode.getUpNode());
                nodeStack.push(currNode.getDownNode());
            } else {
                value += currNode.getPayOff();
                traversed++;
                System.out.println("Position: " + currNode.getTime() + "\t traversed:" + traversed);
            }
        }

        return value;
    }

    public static void main(String[] args) {
        PayoffTree tree = new PayoffTree();
        System.out.println("Value is: " + tree.getValue());

    }
}

class TreeNode {
    private final double S;   // Price of the underlying
    private final int t; // point in time at which we are
    private TreeNode upNode;
    private TreeNode downNode;
    private final TreeNode parent;

    public TreeNode(double price, int time, TreeNode parent) {
        S = price;
        t = time;
        this.parent = parent;
        if (time < PayoffTree.n) {
            upNode = new TreeNode(S * PayoffTree.U, t + 1, this);
            downNode = new TreeNode(S * PayoffTree.D, t + 1, this);
        }
    }

    public TreeNode getParent() {
        return parent;
    }

    public TreeNode getUpNode() {
        return upNode;
    }

    public TreeNode getDownNode() {
        return downNode;
    }

    private double getMean() {
        if (parent == null) return S;
        else return (parent.getMean() * t + S) / (t + 1);
    }

    public double getPayOff() {
        System.out.println(S);
        return Math.max(S - getMean(), 0);
    }

    public int getTime() {
        return t;
    }
}