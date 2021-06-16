package leetCode

import java.io.PrintWriter
import javax.xml.transform.Source

object Main extends App{
    
    def problemset():Unit = {
        val filename = "solved.txt"
        var solvedArray = List(-1)
        for(line <- scala.io.Source.fromFile(filename).getLines()){
            solvedArray = line.split(" ").toList.map(_.toInt)
            var i = 0
        }
        var count = 0
        for(z <- 0 to solvedArray.length - 1){
            if(solvedArray(z) != -1){
                count = count + 1
            }
        }
        var problemN = util.Random.nextInt(400)
        if(count >= 200){
            println("You did 200 questions!! Well done!!")
        }else{
            if(solvedArray.contains(problemN)){
                while(solvedArray.contains(problemN)){
                    problemN = util.Random.nextInt(400)
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

    def problemCheck(n:Int):Unit = {
        val filename = "solved.txt"
        var solvedArray = List(0)
        for(line <- scala.io.Source.fromFile(filename).getLines()){
            solvedArray = line.split(" ").toList.map(_.toInt)
            var i = 0
        }
        if(solvedArray.contains(n)){
            problemset()
        }else{
            println("You can solve the problem " + n)
            val pw = new PrintWriter("solved.txt")
            for(i <- 0 to solvedArray.length - 1){
                pw.print(solvedArray(i) + " ")
            }
            pw.print(n)
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

    //test compleated
    def isPowerOfThree(n:Int): Boolean = {
        if(n == 3){
            true
        }else if(n % 3 != 0){
            false
        }else{
            isPowerOfThree(n/3)
        }
    }

    //test compleated
    def countBits(n:Int): Array[Int] = {
        var ret:Array[Int] = Array.fill(n + 1)(0)
        def helper(n:Int):Array[Int] = {
            if(n >= 0){
                var number = 0
                var count = n.toBinaryString.split("")
                var i = 0
                while(i < count.length){
                    if(count(i) == "1"){
                        number = number + 1
                    }
                    i = i + 1
                }
                ret(n) = number
                helper(n - 1)
            }else{
                ret
            }
        }
        helper(n)
        ret
    }

    //test compleated
    def missingNumber(nums:Array[Int]):Int = {
        var sum = nums.sum
        var max = nums.max
        var idealsum = (max + 1) * max / 2
        if(sum == idealsum){
            max + 1
        }else{
            var ret = idealsum - sum
            ret
        }
    }

    def mergeManually(num1:Array[Int],num2:Array[Int]):Array[Int] = {
        var ret:Array[Int] = Array.fill(num1.length + num2.length)(0)
        var snum1 = num1.sortWith(_ <= _)
        var snum2 = num2.sortWith(_ <= _)
        var ind1 = 0
        var ind2 = 0
        var indr = 0
        while(ind1 < snum1.length || ind2 < snum2.length){
            if(ind1 >= snum1.length){
                ret(indr) = snum2(ind2)
                indr += 1
                ind2 += 1
            }else if(ind2 >= snum2.length){
                ret(indr) = snum1(ind1)
                indr += 1
                ind1 += 1
            }else if(snum1(ind1) <= snum2(ind2)){
                ret(indr) = snum1(ind1)
                ind1 += 1
                indr += 1
            }else if(snum2(ind2) <= snum1(ind1)){
                ret(indr) = snum2(ind2)
                ind2 += 1
                indr += 1
            }
        }
        ret
    }

    //test compleated
    def mergeAuto(num1:Array[Int],num2:Array[Int]):Array[Int] = {
        var ret:Array[Int] = Array.fill(num1.length + num2.length)(0)
        var length = num1.length
        var length2 = num2.length
        if(length > 1 && length2 > 1){
            for(i <- 0 until length){
                ret(i) = num1(i)
            }
            for(i <- length until ret.length){
                ret(i) = num2(i - length)
            }
            ret = ret.sortWith(_ <= _)
            ret
        }else if(length2 > 1){
            ret = num2.sortWith(_ <= _)
        }else if(length > 1){
            ret = num1.sortWith(_ <= _)
        }else{
            ret = Array()
        }
        ret
    }

    //prototype
    def mergeD0P(num1:Array[Int],num2:Array[Int]):Array[Int] = {
        var num1s = num1.sortWith(_ <= _)
        var num2s = num2.sortWith(_ <= _)
        var pos1 = 0
        var pos2 = 0
        while(num1s(pos1) == 0){
            pos1 += 1
        }
        while(num2s(pos2) == 0){
            pos2 += 1
        }
        var ret:Array[Int] = Array.fill(num1s.length + num2s.length - pos1 - pos2)(0)
        if(pos1 != 0 && pos2 != 0){
            for(i <- 0 until num1s.length - pos1){
                ret(i) = num1s(i + pos1)
            }
            for(i <- num1s.length - pos1 until ret.length){
                ret(i) = num2s(i - num1s.length + pos2)
            }
        }else if(pos1 != 0){
            if(num2s.length == 0){
                ret = num1s
            }else{
                for(i <- 0 until num2s.length){
                    ret(i) = num2s(i)
                }
                for(i <- num2s.length until ret.length){
                    ret(i) = num1s(i - num2s.length + pos1)
                }
            }
        }else if(pos2 != 0){
            if(num1s.length == 0){
                ret = num2s
            }else{
                for(i <- 0 until num1s.length){
                    ret(i) = num1s(i)
                }
                for(i <- num1s.length until ret.length){
                    ret(i) = num2s(i - num1s.length + pos2)
                }
            }
        }else{
            if(num1s.length == 0 && num2s.length == 0){
                ret = Array()
            }else if(num1s.length == 0){
                ret = num2s
            }else if(num2s.length == 0){
                ret = num1s
            }else{
                for(i <- 0 until num1s.length){
                    ret(i) = num1s(i)
                }
                for(i <- num1s.length until ret.length){
                    ret(i) = num2s(i - num1s.length)
                }
            }

        }
        ret = ret.sortWith(_ <= _)
        ret
    }
    
    //test compleated
    def mergeD0(nums1:Array[Int],nums2:Array[Int]):Array[Int] = {
        var ret1:Array[Int] = nums2 ++ nums1
        ret1 = ret1.sortWith(_ <= _)
        var count = 0
        while(ret1(count) == 0 && count < ret1.length - 1){
            count += 1
        }
        if(ret1(ret1.length - 1) == 0){
            count += 1
        }
        var ret = Array(0)
        if(ret1.length == count){
            println("All of the elements are 0. There is nothing in the return array.")
        }else{
            ret = Array.fill(ret1.length - count)(0)
            for(i <- 0 until ret.length){
                ret(i) = ret1(i + count)
            }
        }
        ret
    }

    //test compleated
    def po4(n:Int) : Boolean = {
        var res = n % 4
        if(n == 1){
            true
        }else if(res == 0){
            po4(n / 4)
        }else{
            false
        }
    }

    //test compleated
    def myPow(x:Double, n:Int) : Double = {
        def helperp(data:Double,times:Int):Double = {
            if(times == 0){
                1
            }else if(times == 1){
                data
            }else{
                helperp(data*x,times - 1)
            }
        }
        def helpern(data:Double,times:Int):Double = {
            1 / helperp(data,-1 * times)
        }
        if(n >= 0){
            helperp(x,n)
        }else{
            helpern(x,n)
        }
    }

    //test compleated
    def isHappy(n:Int):Boolean = {
        var numbers = List(0)
        def helper(x:Int):Boolean = {
            var narr = x.toString.split("")
            var sum = 0
            for(i <- 0 until narr.length){
                sum = narr(i).toInt*narr(i).toInt + sum
            }
            if(sum == 1){
                true
            }else if(numbers.contains(sum)){
                false
            }else{
                numbers = sum :: numbers
                helper(sum)
            }
        }
        helper(n)
    }

    //test compleated
    def isPalindrome(s:String):Boolean = {
        var letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        var arrStr = s.split("")
        var head = 0
        var tail = arrStr.length - 1
        def helper(h:Int, t:Int): Boolean = {
            if(h >= t){
                true
            }else if(letters.contains(arrStr(h)) && letters.contains(arrStr(t))){
                if(arrStr(h) == arrStr(t)){
                    helper(h+1,t-1)
                }else{
                    false
                }
            }else if(letters.contains(arrStr(h))){
                helper(h,t-1)
            }else if(letters.contains(arrStr(t))){
                helper(h+1,t)
            }else{
                helper(h+1,t-1)
            }
        }
        helper(head,tail)
    }

    //test compleated
    def searchInsert(nums:Array[Int], trg:Int):Int = {
        var snum = nums.sortWith(_<_)
        var len = nums.length
        def find(ind:Int, pre:Int):Int = {
            if(ind == len - 1){
                if(len == 2){
                    if(nums(0) > trg){
                        0
                    }else if(nums(ind) < trg){
                        2
                    }else 1
                }else len
            }else if(nums(ind) < trg && trg < nums(ind+1)){
                ind + 1
            }else if(nums(len - 1)< trg){
                len
            }else if(nums(0) > trg){
                0
            }else if(trg < nums(ind)){
                find(ind/2,ind)
            }else{
                find((pre - ind)/2+ind,ind)
            }
        }
        if(trg == 0){
            0
        }else if(nums.contains(trg)){
            snum.indexOf(trg)
        }else{
            find(len/2,len)
        }
    }

    //test compleated
    def addDigit(n:Int):Int = {
        if(n > 9){
            var dig = n.toString().split("")
            var intarr = Array.fill(dig.length)(0)
            for(i <- 0 until dig.length){
                intarr(i) = dig(i).toInt
            }
            addDigit(intarr.sum)
        }else{
            n
        }
    }

    def generate(row:Int):List[List[Int]] = {
        var ret = List(List(1,1),List(1))
        if(row == 1){
            List(List(1))
        }else if(row == 2){
            ret.reverse
        }else{
            for(i <- 3 until row + 1){
                var newList = Array.fill(i)(1)
                for(n <- 1 until newList.length - 1){
                    newList(n) = ret(i - 2)(n - 1) + ret(i - 2)(n)
                }
                ret = newList.toList :: ret
            }
            ret.reverse
        }
    }

    def factors(num:Int):List[Int] = {
        (1 to num).filter { divisor =>
            num % divisor == 0
        }.toList
    }

    //test compleated
    def myReverse(x:Int):Int = {
        var ret = 0
        if(x < 0){
            var mx = -1 * x
            var intList = mx.toString().split("").toList.map(_.toInt)
            for(i <- 0 until intList.length){
                ret = (myPow(10,i) * intList(i)).toInt + ret
            }
            ret = ret * -1
        }else{
            var intList = x.toString().split("").toList.map(_.toInt)
            for(i <- 0 until intList.length){
                ret = (myPow(10,i) * intList(i)).toInt + ret
            }
        }
        ret
    }
    
    //test compleated
    def moveZeroes(num:Int):List[Int] = {
        var numL = num.toString().split("").toList.map(_.toInt).toArray
        var count = 0
        for(i <- 0 until numL.length){
            if(numL(i) == 0){
                count += 1
            }
        }
        var newL = numL.filter(_ != 0).toList.reverse
        for(i <- 0 until count){
            newL = 0 :: newL
        }
        newL.reverse
    }
    //Test space
    
}
    

