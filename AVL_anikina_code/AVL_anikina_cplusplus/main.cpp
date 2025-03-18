#include <iostream>
#include <iomanip>
using namespace std;

class AVLTree {
private:
    struct Node {
        int key;
        Node* left;
        Node* right;
        int height;

        Node(int k) : key(k), left(nullptr), right(nullptr), height(1) {}
    };

    Node* root;

    int getHeight(Node* node) {
        return node ? node->height : 0;
    }

    int getBalance(Node* node) {
        return node ? getHeight(node->left) - getHeight(node->right) : 0;
    }

    void updateHeight(Node* node) {
        if (node)
            node->height = 1 + max(getHeight(node->left), getHeight(node->right));
    }

    Node* rotateRight(Node* y) {
        Node* x = y->left;
        Node* T2 = x->right;
        x->right = y;
        y->left = T2;
        updateHeight(y);
        updateHeight(x);
        return x;
    }

    Node* rotateLeft(Node* x) {
        Node* y = x->right;
        Node* T2 = y->left;
        y->left = x;
        x->right = T2;
        updateHeight(x);
        updateHeight(y);
        return y;
    }

    Node* balance(Node* node) {
        updateHeight(node);
        int balanceFactor = getBalance(node);

        if (balanceFactor > 1) {
            if (getBalance(node->left) < 0) {
                node->left = rotateLeft(node->left);
            }
            return rotateRight(node);
        }

        if (balanceFactor < -1) {
            if (getBalance(node->right) > 0) {
                node->right = rotateRight(node->right);
            }
            return rotateLeft(node);
        }

        return node;
    }

    Node* insert(Node* node, int key) {
        if (!node) return new Node(key);
        if (key < node->key) node->left = insert(node->left, key);
        else if (key > node->key) node->right = insert(node->right, key);
        else return node;
        return balance(node);
    }

    Node* findMin(Node* node) {
        return node->left ? findMin(node->left) : node;
    }

    Node* remove(Node* node, int key) {
        if (!node) return nullptr;
        if (key < node->key) node->left = remove(node->left, key);
        else if (key > node->key) node->right = remove(node->right, key);
        else {
            Node* left = node->left;
            Node* right = node->right;
            delete node;
            if (!right) return left;
            Node* minRight = findMin(right);
            minRight->right = remove(right, minRight->key);
            minRight->left = left;
            return balance(minRight);
        }
        return balance(node);
    }

    void printTree(Node* root, int depth = 0) {
        if (root == nullptr) return;
        if (root->right) printTree(root->right, depth + 1);

        cout << string(depth * 4, ' ') << root->key << "\n";

        if (root->left) printTree(root->left, depth + 1);
    }

public:
    AVLTree() : root(nullptr) {}

    void insert(int key) {
        root = insert(root, key);
    }

    void remove(int key) {
        root = remove(root, key);
    }

    void printTree() {
        printTree(root);
    }
};

int main() {
    AVLTree tree;
    tree.insert(10);
    tree.insert(20);
    tree.insert(30);
    tree.insert(40);
    tree.insert(50);
    tree.insert(25);

    cout << "AVL tree:\n";
    tree.printTree();

    tree.insert(51);
    tree.insert(1);
    tree.insert(2);

    cout << "\nAVL tree after adding 51, 1 2:\n";
    tree.printTree();

    return 0;
}
