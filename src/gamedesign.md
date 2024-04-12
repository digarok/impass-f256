
main
    init text
    attract message
    key/joy input wait

game
    init level
:mainloop
    get inputs
    move objects
    check collisions
     - this should update object/game states
    draw sprites
    check level end or branch to mainloop

endlevel
    inc level
    if end, post highscore?