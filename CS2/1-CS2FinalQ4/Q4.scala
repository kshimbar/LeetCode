object Main extends App{
    private class Node(var key:A, data:B, left:Node, right:Node)

    private def height(n:Node):Int = {
        if(n == NULL) 0
        else{
            1 + Math.max(height(n.right),height(n.left))
        }
    }

    private def size(n:Node):Int = {
        if(n == NULL){
            0
        }else{
            1 + size(n.left) + size(n.right)
        }
    }
}