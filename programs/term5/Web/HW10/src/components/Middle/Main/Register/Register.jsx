import React, {useRef, useState} from 'react';

const Register = ({users, createUser, setPage}) => {

    const loginInputRef = useRef(null)
    const passwordInputRef = useRef(null)
    const nameInputRef = useRef(null)
    const [error, setError] = useState('')


    const handleRegister = (event) => {
        event.preventDefault()
        const login = loginInputRef.current.value
        const name = nameInputRef.current.value
        const password = passwordInputRef.current.value

        if (login.trim().length === 0 || password.trim().length === 0 || name.trim().length === 0) {
            setError('Password or login or name could not be empty')
            return
        }

        if (3 > login.length || login.length > 16) {
            setError('Login length must be less than 17 and more than 2')
            return
        }

        if (!/^[a-z]+$/.test(login)) {
            setError('Login must contain only little latin characters')
            return
        }

        if (name.length > 32) {
            setError('Name length must be less than 33')
            return
        }

        const alreadyRegistered = users.find((user) => user.login === login)
        if (alreadyRegistered){
            setError("This user is already registered")
        } else {
            createUser({
                name: name,
                login: login,
            })
            setPage('enter')
        }
    }

    return (
        <div className="register form-box">
            <div className="header">Register</div>
            <div className="body">
                <form method="post" action="" onSubmit={handleRegister}>
                    <input type="hidden" name="action" value="enter"/>
                    <div className="field">
                        <div className="name">
                            <label htmlFor="login">Login</label>
                        </div>
                        <div className="value">
                            <input
                                autoFocus
                                id="login"
                                name="login"
                                ref={loginInputRef}
                                onChange={() => setError(null)}
                            />
                        </div>
                    </div>
                    <div className="field">
                        <div className="name">
                            <label htmlFor="name">Name</label>
                        </div>
                        <div className="value">
                            <input
                                autoFocus
                                id="name"
                                name="name"
                                ref={nameInputRef}
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
                        <input type="submit" value="Register"/>
                    </div>
                </form>
            </div>
        </div>
    );
};

export default Register;