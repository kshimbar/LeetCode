package leetCode

object Main extends App{

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
    //Test space
    println(romanToInt("MMXLIIX"))
}
    

