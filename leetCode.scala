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
    //Test space
    
}

