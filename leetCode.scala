package leetCode

import java.io.PrintWriter
import javax.xml.transform.Source

object Main extends App{
    
    def problemset():Unit = {
        val filename = "solved.txt"
        var solvedArray:Array[Int] = Array.fill(100)(-1)
        for(line <- scala.io.Source.fromFile(filename).getLines()){
            var solved = line.split(" ")
            var i = 0
            while(i < solved.length){
                solvedArray(i) = solved(i).toInt
                i = i + 1
            }
        }
        var count = 0
        for(z <- 0 to solvedArray.length - 1){
            if(solvedArray(z) != -1){
                count = count + 1
            }
        }
        var problemN = util.Random.nextInt(100)
        println(problemN)
        if(count >= 100){
            println("You did 100 questions!! Well done!!")
        }else{
            if(solvedArray.contains(problemN)){
                while(solvedArray.contains(problemN)){
                    problemN = util.Random.nextInt(100)
                }
            }
            println("The problem you will solve next is " + problemN)
            val pw = new PrintWriter("solved.txt")
            for(i <- 0 to solvedArray.length - 1){
                if(solvedArray(i) != -1){
                    pw.print(solvedArray(i) + " ")
                }
            }
            pw.print(problemN)
            pw.close()
            pw.flush()
        }
    }
    //Test compleated.
    def twoSum(nums: Array[Int], target: Int): Unit = {
        var i = 0
        var checker = 0
        while(i < nums.length - 1){
            var n = i + 1
            while (n<nums.length){
                if(nums(i) + nums(n) == target && checker == 0){
                    println("You find the pair. It is " + nums(i) + " and " + nums(n))
                    checker += 1
                }
                n += 1
            }
            i += 1
        }
        if(checker == 0){
            println("There is no such a pair.")
        }
    }

    //test compleated
    def validParentheses(str:String):Unit = {
        var prentheses = 0
        var curlyBrackets = 0
        var squareBrackets = 0
        val strArr:Array[String] = str.split("")
        var i = 0
        while(i < strArr.length){
            if(strArr(i) == "("){
                prentheses += 1
            }else if(strArr(i) == ")"){
                prentheses -= 1
            }else if(strArr(i) == "}"){
                curlyBrackets += 1
            }else if(strArr(i) == "{"){
                curlyBrackets -= 1
            }else if(strArr(i) == "]"){
                squareBrackets += 1
            }else if(strArr(i) == "["){
                squareBrackets -= 1
            }
            i += 1
        }
        if(prentheses == 0 && curlyBrackets == 0 && squareBrackets == 0){
            println("this is valid")
        }else{
            println("invalid input")
        }
    }

    def rti(s:String):Int = {
        var ret = 0
        if(s == "I"){
            ret = 1
        }else if(s == "V"){
            ret = 5
        }else if(s == "X"){
            ret = 10
        }else if(s == "L"){
            ret = 50
        }else if(s == "C"){
            ret = 100
        }else if(s == "D"){
            ret = 500
        }else if(s == "M"){
            ret = 1000
        }
        ret
    }

    //test conleated
    def romanToInt(s:String):Int = {
        val arrString = s.split("")
        var i = 0
        var total = 0
        if(arrString.length == 1){
            total = rti(arrString(0))
        }else{
            while(i < arrString.length - 1){
                var cur = rti(arrString(i))
                var next = rti(arrString(i + 1))
                total = total + cur
                if(cur < next){
                    println("check")
                    var minus = rti(arrString(i))
                    if(i == 0){
                        minus = rti(arrString(0))
                    }else{
                        var now = rti(arrString(i))
                        var pre = rti(arrString(i - 1))
                        var n = i - 1
                        while(now >= pre && n >= 0){
                            now = pre
                            if(n > 0){
                                pre = rti(arrString(n - 1))
                            }
                            n = n - 1
                            minus = minus + now
                        }
                    }
                    total = total - minus - minus
                }
                i = i + 1
            }
            total = total + rti(arrString(arrString.length - 1))
        }
        total
    }

    //test compleated
    class TreeNode(_value:Int = 0, _left: TreeNode = null, _right: TreeNode = null){
        var value:Int = _value
        var left = _left
        var right = _right
    }

    def isSameTree(p:TreeNode, q:TreeNode) : Boolean = {
        if(p == null && q == null){
            true
        }else if(p == null || q == null){
            false
        }else{
            if(p.value != q.value){
                false
            }else{
                isSameTree(p.right,q.right) && isSameTree(p.left, q.left)
            }
        }
    }

    def isPowerOfThree(n:Int): Boolean = {
        if(n == 3){
            true
        }else if(n % 3 != 0){
            false
        }else{
            isPowerOfThree(n/3)
        }
    }
    //Test space
    problemset()
}
    

