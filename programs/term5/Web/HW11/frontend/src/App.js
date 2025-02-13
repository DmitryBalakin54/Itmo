import './App.css';
import Enter from "./components/Middle/Main/Enter/Enter";
import Index from "./components/Middle/Main/Index/Index";
import React, {useEffect, useMemo, useState} from "react";
import {BrowserRouter, Route, Routes} from "react-router-dom";
import Application from "./Application";
import axios from "axios";
import WritePost from "./components/Middle/Main/WritePost/WritePost";
import Users from "./components/Middle/Main/Users/Users";
import NotFound from "./components/Middle/Main/NotFound/NotFound";
import Posts from "./components/Middle/Main/Posts/Posts";
import Register from "./components/Middle/Main/Register/Register";
import Post from "./components/Middle/Main/Posts/Post/Post";

function App() {

    const [login, setLogin] = useState(null)
    const [posts, setPosts] = useState(null)

    useEffect(() => {
        if (localStorage.getItem("jwt")){
            axios.get("/api/jwt", {
                params: {
                    jwt: localStorage.getItem("jwt")
                }
            }).then((response)=>{
                localStorage.setItem("login", response.data.login);
                setLogin(response.data.login)
            }).catch((error)=>{
                console.log(error)
            })
        }
    }, []);

    useEffect(() => {
        axios.get("/api/posts").then((response)=>{
            setPosts(response.data)
        }).catch((error)=>{
            console.log(error)
        })
    }, [setPosts]);

    const isAuth = useMemo(() => {
        return !!login;
    }, [login])

    return (
        <div className="App">
            <BrowserRouter>
                <Routes>
                    <Route
                        index={true}
                        element={<Application isAuth={true} setLogin={setLogin} login={login} posts={posts} setPosts={setPosts} page={<Index/>}/>}
                    />
                    <Route
                        exact path={'/enter'}
                        element={<Application isAuth={true} login={login} setLogin={setLogin} posts={posts} setPosts={setPosts} page={<Enter setLogin={setLogin}/>}/>}
                    />
                    <Route
                        exact path={'/users'}
                        element={<Application isAuth={isAuth} login={login} setLogin={setLogin} posts={posts} setPosts={setPosts} page={<Users/>}/>}
                    />
                    <Route
                        exact path={'/posts'}
                        element={<Application isAuth={true} login={login} setLogin={setLogin} posts={posts} setPosts={setPosts} page={<Posts/>}/>}
                    />
                    <Route
                        exact path={'/register'}
                        element={<Application isAuth={true} login={login} setLogin={setLogin} posts={posts} setPosts={setPosts} page={<Register setLogin={setLogin}/>}/>}
                    />
                    <Route
                        exact path={'/writePost'}
                        element={<Application isAuth={isAuth} login={login} posts={posts} setPosts={setPosts} setLogin={setLogin} page={<WritePost posts={posts} setPosts={setPosts} userLogin={login} />}/>}
                    />
                    <Route
                        exact path={'/posts/:id'}
                        element={<Application isAuth={true} login={login} posts={posts} setPosts={setPosts} setLogin={setLogin} page={<Post isOnlyOne={true}/>} />}
                    />
                    <Route path="*"
                           element={<Application isAuth={false} login={login} posts={posts} setPosts={setPosts} setLogin={setLogin} page={<NotFound/>}/>}
                    />
                </Routes>
            </BrowserRouter>
        </div>
    );
}

export default App;
