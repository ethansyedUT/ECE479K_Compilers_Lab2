class Main{
    main() : Int {0};

};
class Dog inherits Animal{
    x : Int;
    bark() : Int {15};
    get_age() : Int {10};
};
class Animal{
    y : Int <- 2;
    shark : Int;
    name : String;
    get_age() : Int {10};
    get_name() : Int {0};
};
class Hound inherits Dog{
    get_age() : Int {11};
    loudBark() : Int {10};
    louderBark() : Int {10};
    loudestBark() : Int {10};
};
class MedStudent{
    get_score() : Int {10};
};
class EngStudent inherits IO{
    get_autism_score() : Int {10};
};

-- String return from method causes seg fault