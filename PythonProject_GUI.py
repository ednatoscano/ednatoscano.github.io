"""
COMP.CS.100 Ohjelmointi 1 / Programming 1

Student Id: 150167534
Name:       Mikel Robredo Manero
Email:      pqmiro@tuni.fi

Student Id: 150360467
Name:       Edna Toscano
Email:      edna.toscanocabrera@tuni.fi

13.10 ⌚⌚⌚ Project: Graphical User Interface - POKEMON MEMORY CARD GAME

OBJECT OF THE GAME: Find the most pairs of matching pokemon cards.
GAMEPLAY:
    - The pokeball cards will be shuffled at the very beginning.
    - The player turns over any two cards (one card at a time).
    - If the pair of cards don't match, then these must be turn down to
    continue the game.
    - If the pair of cards match, then the player continues with another two
    cards.
    - And so on until all the pair of cards have been discovered.
"""

import random
from tkinter import *

MAX_ROW = 4
MAX_COLUMN = 9

TOTAL_NUM_BUTTONS = 36

NUMBER_OF_POKEMONS = 18


class MemoryGame:

    def __init__(self):
        """
        This method initializes all the content inside the Tkinter displayed
        interface.
        """
        # Sets the Tkinter interface window.
        self.__main_window = Tk()

        # List of all the pairs of pokemon image files.
        self.__IMAGE_FILES = ["1.png", "1.png", "2.png", "2.png", "3.png",
                              "3.png",
                              "4.png", "4.png", "5.png", "5.png", "6.png",
                              "6.png",
                              "7.png", "7.png", "8.png", "8.png", "9.png",
                              "9.png",
                              "10.png", "10.png", "11.png", "11.png", "12.png",
                              "12.png",
                              "13.png", "13.png", "14.png", "14.png", "15.png",
                              "15.png",
                              "16.png", "16.png", "17.png", "17.png", "18.png",
                              "18.png"]

        # We initialize empty auxiliary lists to manage the location of images,
        # buttons and more...

        # It's the list of pokemons clicked in each selected button.
        self.__pokemon_clicked_name_list = []
        # List containing each row and column of each selected button.
        self.__coordinates_list = []
        # First and second components of the pair of pokeball cards, based on
        # the locations given in coordinates_list.
        self.__button_pokemon1 = []
        self.__button_pokemon2 = []
        # Counter of matching pairs.
        self.__match_counter = 0
        # List of card names that matched.
        self.__match_pokemons = []

        # Contains the photo files of each of the pokemons
        self.__pokemon_list = []
        self.__pokemon_names_list = []

        self.__main_instructions = "Instruction: Turn over two pokemon cards " \
                                   "and see if they match"
        title_image = PhotoImage(file="pokemon.png")
        self.__main_title = Label(self.__main_window, image=title_image)
        self.__secondary_title = Label(self.__main_window,
                                       text="Find the 18 pairs of matching "
                                            "Pokemon cards!",
                                       background="orange",
                                       font=("Arial Black", 25, "bold"))
        self.__main_window.title("Memory Game")

        # Button to turn down the cards selected.
        self.__turn_cards_down = Button(self.__main_window,
                                        text="  Turn down cards  ",
                                        command=self.turn_down,
                                        background="deepskyblue3",
                                        state=DISABLED,
                                        font=("Calibri", 11, "bold"))

        # Button to restart the whole game and shuffle the cards.
        self.__restart_button = Button(self.__main_window,
                                       text="     Restart game     ",
                                       bg="gold1", state=DISABLED,
                                       command=self.restart_game,
                                       font=("Calibri", 11, "bold"))

        # Images used at the end of the game when the user wins.
        self.__empty_image = PhotoImage(width=120, height=120)
        self.__ash_image = PhotoImage(file="Ash.png")
        self.__brock_image = PhotoImage(file="Brock.png")
        self.__ash_trainer = Label(self.__main_window,
                                   image=self.__empty_image)
        self.__brock_trainer = Label(self.__main_window,
                                     image=self.__empty_image)
        self.__pokeball_image = PhotoImage(file="pokeball.png")

        # Button to quit the game.
        self.__quit = Button(self.__main_window,
                             text="  QUIT ",
                             command=self.quit, background="firebrick1",
                             font=("Arial Black", 9))

        self.__buttons_matrix = []

        # Following for loop will initialize the list __buttons to be
        # MAX_ROW x MAX_COLUMN matrices (lists within a list) with all elements
        # initialized as None. This list will contain bookkeeping information
        # about the location of the games clicked buttons connected to a button
        # in a particular location on the board.
        for row in range(0, MAX_ROW):
            self.__buttons_matrix.append([None] * MAX_COLUMN)

        # Method that shuffles the order of all images and buttons.
        self.shuffle_cards()

        # We generate MAX_ROW * MAX_COLUMN command functions and click_buttons,
        # and store the information about them in the matrix __buttons_matrix.
        for row in range(0, MAX_ROW):
            for column in range(0, MAX_COLUMN):
                # Command functions to handle a button press on the game in
                # coordinates [row][column].
                def button_press(button_row=row, button_column=column):
                    self.open_pokeball(button_row, button_column)
                    self.check_pokeball(button_row, button_column)

                # The new button is placed on coordinates [row][column] and its
                # command function is the one defined earlier, which gets as
                # its parameter the location [row][column] of the button click.
                pokemon_button = Button(self.__main_window,
                                        image=self.__pokeball_image,
                                        command=button_press)
                # The newly created button is also stored into the
                # bookkeeping matrix.
                self.__buttons_matrix[row][column] = pokemon_button
                pokemon_button.grid(row=row + 7, column=column)

        # List of buttons from __buttons_matrix stored linearly for easy
        # handling.
        self.__list_of_buttons_matrix = []

        # Fills the previously created list with the objects from
        # __buttons_matrix.
        for i in range(0, MAX_ROW):
            for buttons in self.__buttons_matrix[i][:]:
                self.__list_of_buttons_matrix.append(buttons)

        # Positioning part for all the components. (4 ROWS (from 0 to 4) &
        # 9 COLUMNS (from 0 to 9))
        self.__main_title.grid(row=0, column=0, columnspan=9)
        self.__ash_trainer.grid(row=0, column=2, rowspan=3, sticky=S)
        self.__brock_trainer.grid(row=0, column=6, rowspan=3, sticky=S)
        self.__secondary_title.grid(row=11, column=0, columnspan=9,
                                    sticky=W+E)
        self.__restart_button.grid(row=1, column=7, columnspan=3, sticky=E)
        self.__turn_cards_down.grid(row=2, column=7, columnspan=3, sticky=E)
        self.__quit.grid(row=0, column=8, columnspan=1, sticky=N+E)

        # Maintains the window in a constant loop.
        self.__main_window.mainloop()

    def shuffle_cards(self):
        """
        Shuffles the list of images files and generates lists for:
            - The PhotoImage object obtained from the __IMAGE_FILES list.
            - The names of the images in order to identify the buttons.
        """

        random.shuffle(self.__IMAGE_FILES)

        # Initializes the lists for each content mentioned in the method
        # description.
        pokemon_photo_image = []
        pokemon_names = []

        for pokemon in self.__IMAGE_FILES:
            pokemon_photo_image.append(PhotoImage(file=pokemon,  width=130, height=130))
            pokemon_names.append(pokemon)

        self.__pokemon_list.clear()
        self.__pokemon_names_list.clear()

        # Local lists pokemon_photo_image & pokemon_names work as auxiliary
        # lists in order to fill the __pokemon_list and __pokemon_names_list
        # attributes, which will be used all over the entire game/program.
        for i in range(0, len(pokemon_photo_image), MAX_COLUMN):
            self.__pokemon_list.append(
                pokemon_photo_image[i:i + MAX_COLUMN])
            self.__pokemon_names_list.append(
                pokemon_names[i:i + MAX_COLUMN])

    def open_pokeball(self, button_row, button_column):
        """
        This method configures the button click to show a pokemon image
        instead of the pokeball and removes the functions of the command from
        the button clicked.

        :param button_row: int, row location of the clicked button from the
        __buttons_matrix.
        :param button_column: int, column location of the clicked button from
        the __buttons_matrix.
        """

        pokemon_selected = self.__pokemon_list[button_row][button_column]
        activated_button = self.__buttons_matrix[button_row][button_column]
        activated_button.configure(image=pokemon_selected)
        activated_button['command'] = 0


    def check_pokeball(self, button_row, button_column):
        """
        OVERVIEW: Checks if the pair of buttons clicked are match or not.

        :param button_row: int, row location of the clicked button from the
        __buttons_matrix.
        :param button_column: int, column location of the clicked button from
        the __buttons_matrix.
        """

        # This list is filled each time two cards are clicked. It's used for
        # checking if the names of the two images in the buttons are match or
        # not.
        self.__pokemon_clicked_name_list.append(
            self.__pokemon_names_list[button_row][button_column])
        # Stores the location of the each Button Object clicked in the
        # __buttons_matrix.
        self.__coordinates_list.append([button_row, button_column])

        # When only one button is clicked, the buttons __turn_cards_down &
        # __restart_button are disabled.
        if len(self.__pokemon_clicked_name_list) == 1:

            self.__turn_cards_down["state"] = DISABLED
            self.__restart_button["state"] = DISABLED

        # When two buttons are clicked, we compare if the images match or not.
        elif len(self.__pokemon_clicked_name_list) == 2:

            self.__restart_button["state"] = NORMAL

            # Row and Column location of each button.
            pokemon1_row, pokemon1_column = self.__coordinates_list[0][0], \
                                            self.__coordinates_list[0][1]
            pokemon2_row, pokemon2_column = self.__coordinates_list[1][0], \
                                            self.__coordinates_list[1][1]

            # The Button object of the location fetched from the
            # __buttons_matrix.
            self.__button_pokemon1 = self.__buttons_matrix[pokemon1_row][
                pokemon1_column]
            self.__button_pokemon2 = self.__buttons_matrix[pokemon2_row][
                pokemon2_column]

            # If the images match, then their button objects are disabled and
            # the background colour turns green.
            if self.__pokemon_clicked_name_list[0] == \
                    self.__pokemon_clicked_name_list[1]:

                self.__match_pokemons.append(self.__button_pokemon1)
                self.__match_pokemons.append(self.__button_pokemon2)

                self.__button_pokemon1.configure(state=DISABLED, bg="green")
                self.__button_pokemon2.configure(state=DISABLED, bg="green")

                # We restore the command functions of the buttons clicked, in
                # case the game is re-started. This change won't be notices
                # since the state of the button is disabled.
                self.__button_pokemon1.configure(command=lambda: [
                    self.open_pokeball(pokemon1_row, pokemon1_column),
                    self.check_pokeball(pokemon1_row, pokemon1_column)])
                self.__button_pokemon2.configure(command=lambda: [
                    self.open_pokeball(pokemon2_row, pokemon2_column),
                    self.check_pokeball(pokemon2_row, pokemon2_column)])

                # Auxiliary lists used for the two buttons clicked are cleared
                # for next buttons.
                self.__pokemon_clicked_name_list.clear()
                self.__coordinates_list.clear()

                self.__turn_cards_down["state"] = DISABLED
                self.__secondary_title.configure(
                    text="""You got a match. Turn over two more pokeballs""")

                # We store the amount of matches in order to know when the user
                # wins the game.
                self.__match_counter += 1

                if self.__match_counter == 2:
                    self.__secondary_title.configure(
                        text="Two matches in a row, you are killing it!")
                elif self.__match_counter == 3:
                    self.__secondary_title.configure(
                        text="Another match! Nothing can stop you!")
                elif self.__match_counter == 4:
                    self.__secondary_title.configure(
                        text="You are unstoppable!")
                elif self.__match_counter == 5:
                    self.__secondary_title.configure(
                        text="Seriously, how are you this good!")

                # If the player gets all the matches, then following changes
                # are made in the interface.
                if len(self.__match_pokemons) == TOTAL_NUM_BUTTONS:
                    self.__secondary_title.configure(text="YOU WIN! "
                                                          "CONGRATULATIONS!")
                    self.__ash_trainer["image"] = self.__ash_image
                    self.__brock_trainer["image"] = self.__brock_image

                    for button in self.__list_of_buttons_matrix:
                        button.configure(bg="purple")

            # If the pair of cards don't match, then their command functions
            # for the buttons are restored.
            else:

                self.__button_pokemon1.configure(command=lambda: [
                    self.open_pokeball(pokemon1_row, pokemon1_column),
                    self.check_pokeball(pokemon1_row, pokemon1_column)])
                self.__button_pokemon2.configure(command=lambda: [
                    self.open_pokeball(pokemon2_row, pokemon2_column),
                    self.check_pokeball(pokemon2_row, pokemon2_column)])

                # Also, in order to continue with the game,
                # __turn_cards_down button must be pressed, its state
                # changes to Normal.
                self.__turn_cards_down["state"] = NORMAL
                self.__secondary_title.configure(
                    text="""Not a match, press the button 'Turn down cards'""")

                # Disables all the buttons from the interface.
                for pokemon_buttons in self.__list_of_buttons_matrix:
                    if pokemon_buttons != self.__button_pokemon1 or \
                            pokemon_buttons != self.__button_pokemon2:
                        pokemon_buttons['state'] = DISABLED

                self.__pokemon_clicked_name_list.clear()
                self.__coordinates_list.clear()
                self.__match_counter = 0

    def turn_down(self):
        """
        This method turns down all the cards except the matched cards.
        """

        # Gives again the image of the pokeball to the cards that didn't match.
        self.__button_pokemon1.configure(image=self.__pokeball_image)
        self.__button_pokemon2.configure(image=self.__pokeball_image)
        self.__secondary_title.configure(text="Turn over two pokemon "
                                           "cards and see if they match")

        # Activates the state of all the buttons from the interface.
        for i in range(0, 4):
            for pokemon_button in self.__buttons_matrix[i]:
                pokemon_button['state'] = NORMAL

        # BUT, it disables again the cards that did match.
        for match_button in self.__match_pokemons:
            match_button['state'] = DISABLED

    def restart_game(self):
        """
        OVERVIEW: This method resets the game.
        """

        # All matched buttons return to their normal state and background
        # colour.
        for match_button in self.__match_pokemons:
            match_button.configure(bg="SystemButtonFace", state=NORMAL)

        # Restore the secondary title text
        self.__secondary_title.configure(text="Find the 18 pairs of matching Pokemon cards!")

        # Pokeball images is restored in all the buttons from the interface.
        # At the same time, the state of the buttons is changed to normal.
        for row in range(0, MAX_ROW):
            for column in range(0, MAX_COLUMN):
                self.__buttons_matrix[row][column].configure(
                    image=self.__pokeball_image, state=NORMAL)

        # Cards are shuffled again using the method explained before.
        self.shuffle_cards()

        self.__ash_trainer["image"] = self.__empty_image
        self.__brock_trainer["image"] = self.__empty_image

        # All auxiliary lists are cleared for next game.
        self.__pokemon_clicked_name_list.clear()
        self.__coordinates_list.clear()
        self.__match_pokemons.clear()
        self.__match_counter = 0


    def quit(self):
        """
        Terminates the program.
        """
        self.__main_window.destroy()


def main():
    MemoryGame()


if __name__ == "__main__":
    main()
