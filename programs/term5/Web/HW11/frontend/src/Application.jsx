import './App.css';
import React, {useEffect, useState} from "react";
import Middle from "./components/Middle/Middle";
import Footer from "./components/Footer/Footer";
import Header from "./components/Header/Header";
import axios from "axios";
import NotFound from "./components/Middle/Main/NotFound/NotFound";

function Application({page, login, setLogin, isAuth, posts, setPosts}) {

    return (
        <div>
            <Header setLogin={setLogin} login={login}/>
            {isAuth ?
                <Middle
                    posts={posts}
                    page={page}
                />
                :
                <NotFound/>
            }
            <Footer/>
        </div>
    );
}

export default Application;
