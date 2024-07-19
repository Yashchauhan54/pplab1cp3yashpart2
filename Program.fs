

//lab1
//Q2.Answer
let rec printyashmovieList list =
    match list with
    | [] -> ()
    | yes :: no ->
        printfn "%A" yes
        printyashmovieList no
 
let listofmovie = ["Jawan-20$"; "Spiderman-10$"; "Koi mill gaya-30$";"Tiger zinda hai-40$"]
printyashmovieList listofmovie


//Q3.Answer
 
let productOfyashodd (n: int) = 
    let rec countodd (current: int) (acc: int) =         
      if current <= 0 then acc         
      else countodd (current - 2) (acc * current)    
    countodd n 1 
 
let res = productOfyashodd 11

printfn "Q3.Answer: The product of all odd numbers from 11 to 1 is %d" res

//Q4.Answer

let trimnameyash (givenList: string list) = 
    givenList |> List.map (fun x -> x.Trim()) 
 
let list = [" Charles"; "Babbage  "; "  Von Neumann  "; "  Dennis Ritchie  "] 
let newtrimmedNames = trimnameyash list 
 
newtrimmedNames |> List.iter (printfn "%s") 
printfn "Q4.Answer: trimmed list: %A" newtrimmedNames


//Q5
let yashsequence = seq {1 .. 700}
let numberList = Seq.toList yashsequence
let myfilteredList = List.filter (fun x -> x % 7 = 0 && x % 5 = 0) numberList

let additionOfFilteredNumbers = List.fold (+) 0 myfilteredList

printfn "Filtered numbers: %A" myfilteredList

printfn "Q5.Answer: The sum of all numbers that are multiples of both 7 and 5 is %d" additionOfFilteredNumbers







//Q6
let yashgivennames: string list = ["James";"Robert";"John";"William";"Michael";"David";"Richard"] 

let newfilteredNames: string list =  
    List.filter (fun name -> name.Contains("i", 
    System.StringComparison.OrdinalIgnoreCase)) yashgivennames 
 
let mynewconcatenatedNames: string =  
    List.fold (fun acc name -> acc + name) "" newfilteredNames 
printfn "Q6.Answer: The new Concatenated names containing 'i': %s" mynewconcatenatedNames  





