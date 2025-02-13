import React, {useCallback, useRef, useState} from 'react';

const Enter = ({users, setUser, setPage}) => {
    const loginInputRef = useRef(null)
    const passwordInputRef = useRef(null)
    const [error, setError] = useState(null)

    const onEnter = useCallback(() => {
        const login = loginInputRef.current.value
        const password = passwordInputRef.current.value

        if (login.trim().length === 0 || password.length === 0) {
            setError('Password or login could not be empty')
        }
        const loggedIn = users.find((user) => user.login === login)
        if (!loggedIn){
            setError("Wrong login or password")
        } else {
            setUser(loggedIn)
            setPage('index')
        }
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