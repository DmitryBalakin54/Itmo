import React from 'react';
import EnterOrRegister from "./EnterOrRegister/EnterOrRegister";
import Translations from "./Translations/Translations";
import Navigation from "./Navigation/Navigation";

const Header = ({login, setLogin}) => {

    return (
        <header>
            <Translations/>
            <EnterOrRegister login={login} setLogin={setLogin}/>
            <Navigation login={login}/>
        </header>
    );
};

export default Header;