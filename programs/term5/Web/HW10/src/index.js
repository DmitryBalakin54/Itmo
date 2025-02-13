import React from 'react';
import ReactDOM from 'react-dom/client';
import App from './App';
import {data} from "./data";

const root = ReactDOM.createRoot(document.getElementById('root'));
root.render(
    <React.StrictMode>
        <App usersData={data.users} postsData={data.posts}/>
    </React.StrictMode>
);

