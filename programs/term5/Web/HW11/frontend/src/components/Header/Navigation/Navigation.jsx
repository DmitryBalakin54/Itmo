import React from 'react';
import {useNavigate} from "react-router-dom";

const Navigation = ({login}) => {

    const router = useNavigate()

    return (
        <nav>
            <ul>
                <li>
                    <a href="" onClick={(event) => {
                        event.preventDefault()
                        router("/")
                    }}>Home</a>
                </li>
                <li><a href="" onClick={(event)=>{
                    router('/users')
                    event.preventDefault()
                }}>Users</a></li>
                {login
                    ?
                    <li>
                        <a href="" onClick={(event)=>{
                            router('/writePost')
                            event.preventDefault()
                        }}>
                            Write Post
                        </a>
                    </li>
                    : null
                }
                <li><a href="" onClick={(event)=>{
                    router('/posts')
                    event.preventDefault()
                }}>Posts</a></li>
            </ul>
        </nav>
    );
};

export default Navigation;