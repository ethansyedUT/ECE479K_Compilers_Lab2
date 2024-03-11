class Main {
    main() : Int {{
        (let x:Int in
            while x < 10 loop
                x <- 10
            pool
        );
        0;
    }};
};