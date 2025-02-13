<#-- @ftlvariable name="talks" type="ru.itmo.wp.model.domain.Talk[]" -->
<#-- @ftlvariable name="targetUserId" type="java.lang.Long" -->
<#-- @ftlvariable name="text" type="java.lang.String" -->
<#-- @ftlvariable name="users" type="ru.itmo.wp.model.domain.User[]" -->

<#import "commons.ftlh" as c>

<@c.page>
    <div class="form-box" style="margin-bottom: 2rem">
        <div class="header">Send message</div>
        <div class="body">
            <form method="post" action="">
                <input type="hidden" name="action" value="send"/>

                <div class="field">
                    <div class="name">
                        <label for="targetLogin">Select user</label>
                    </div>
                    <div class="value">
                        <select id="targetLogin" name="targetLogin" required>
                            <option value="">-- Select User --</option>
                            <#list users as user>
                                <option value="${user.id}" <#if targetUserId?? && targetUserId == user.id>selected</#if>>
                                    ${user.login}
                                </option>
                            </#list>
                        </select>
                    </div>
                </div>

                <div class="field">
                    <div class="name">
                        <label for="text">Message</label>
                    </div>
                    <div class="value">
                        <textarea id="text" name="text" rows="7" style="width: 100%; resize: none;" required>${text!}</textarea>
                    </div>
                </div>

                <div class="button-field">
                    <input type="submit" value="Send">
                </div>
            </form>
        </div>
    </div>

    <div class="datatable">
        <div class="caption">Talks</div>
        <table>
            <thead>
            <tr>
                <th>Id</th>
                <th>From</th>
                <th>To</th>
                <th>Text</th>
                <th>Created</th>
            </tr>
            </thead>
            <tbody>
            <#list talks as talk>
                <tr>
                    <td>${talk.id}</td>
                    <td>${talk.sourceLogin}</td>
                    <td>${talk.targetLogin}</td>
                    <td class="text-cell" >${talk.text}</td>
                    <td>${talk.creationTime}</td>
                </tr>
            </#list>
            </tbody>
        </table>
    </div>
</@c.page>
