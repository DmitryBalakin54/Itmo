import React, {useCallback, useRef, useState} from 'react';
import axios from "axios";
import {useNavigate} from "react-router-dom";

const Enter = ({ setLogin}) => {

    const loginInputRef = useRef(null)
    const passwordInputRef = useRef(null)
    const [error, setError] = useState(null)

    const router = useNavigate()

    const onEnter = useCallback(() => {
        const login = loginInputRef.current.value
        const password = passwordInputRef.current.value

        if (login.trim().length === 0 || password.trim().length === 0) {
            setError('Password or login could not be empty')
            return
        }
        axios.post("/api/jwt", {
            login: login,
            password: password
        }).then((response)=>{
            const jwt = response.data
            localStorage.setItem("jwt", jwt)
            axios.get("/api/jwt", {
                params: {
                    jwt: jwt
                }
            }).then((response)=>{
                setLogin(response.data.login)
                router("/");
            }).catch((error)=>{
                console.log(error)
            })
        }).catch((error)=>{
            setError(error.response.data)
        })

    }, [])

    return (
        <div className="enter form-box">
            <div className="header">Enter</div>
            <div className="body">
                <form method="" action="" onSubmit={event => {
                    event.preventDefault()
                    onEnter()
                }}>
                    <input type="hidden" name="action" value="enter"/>
                    <div className="field">
                        <div className="name">
                            <label htmlFor="login">Login</label>
                        </div>
                        <div className="value">
                            <input
                                autoFocus
                                name="login"
                                ref={loginInputRef}
                                onChange={() => setError(null)}
                            />
                        </div>
                    </div>
                    <div className="field">
                        <div className="name">
                            <label htmlFor="password">Password</label>
                        </div>
                        <div className="value">
                            <input
                                name="password"
                                type="password"
                                ref={passwordInputRef}
                                onChange={() => setError(null)}
                            />
                        </div>
                    </div>
                    {error
                        ? <div className={'error'}>{error}</div>
                        : null
                    }
                    <div className="button-field">
                        <input type="submit" value="Enter"/>
                    </div>
                </form>
            </div>
        </div>
    );
};

export default Enter;