import React from 'react';
import EnterOrRegister from "./EnterOrRegister/EnterOrRegister";
import Translations from "./Translations/Translations";
import Navigation from "./Navigation/Navigation";

const Header = ({user, setUser, setPage}) => {

    return (
        <header>
            <Translations/>
            <EnterOrRegister user={user} setUser={setUser} setPage={setPage}/>
            <Navigation user={user} setPage={setPage}/>
        </header>
    );
};

export default Header;