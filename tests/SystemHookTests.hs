{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module SystemHookTests (systemHookTests) where

import Control.Monad.IO.Class
import qualified Data.Text.IO as TIO
import GitLab
import Test.Tasty
import Test.Tasty.HUnit

systemHookTests :: [TestTree]
systemHookTests =
  [parserTests, matchTests, matchIfTests, receiveTests]

parserTests :: TestTree
parserTests =
  testGroup
    "GitLab system hook rules"
    [ testCase
        "project-create-event"
        ( TIO.readFile "data/system-hooks/project-created.json"
            >>= \eventJson -> parseEvent eventJson @?= Just projectCreatedHaskell
        ),
      testCase
        "project-create-diacritics-event"
        ( TIO.readFile "data/system-hooks/project-created-diacritics.json"
            >>= \eventJson -> parseEvent eventJson @?= Just projectCreatedDiatricsHaskell
        ),
      testCase
        "project-destroy-event"
        ( TIO.readFile "data/system-hooks/project-destroyed.json"
            >>= \eventJson -> parseEvent eventJson @?= Just projectDestroyedHaskell
        ),
      testCase
        "project-rename-event"
        ( TIO.readFile "data/system-hooks/project-renamed.json"
            >>= \eventJson -> parseEvent eventJson @?= Just projectRenamedHaskell
        ),
      testCase
        "project-transfer-event"
        ( TIO.readFile "data/system-hooks/project-transferred.json"
            >>= \eventJson -> parseEvent eventJson @?= Just projectTransferredHaskell
        ),
      testCase
        "project-update-event"
        ( TIO.readFile "data/system-hooks/project-updated.json"
            >>= \eventJson -> parseEvent eventJson @?= Just projectUpdatedHaskell
        ),
      testCase
        "new-team-member-event"
        ( TIO.readFile "data/system-hooks/new-team-member.json"
            >>= \eventJson -> parseEvent eventJson @?= Just userAddedToTeamHaskell
        ),
      testCase
        "team-member-removed-event"
        ( TIO.readFile "data/system-hooks/team-member-removed.json"
            >>= \eventJson -> parseEvent eventJson @?= Just userRemovedFromTeamHaskell
        ),
      testCase
        "team-member-updated-event"
        ( TIO.readFile "data/system-hooks/team-member-updated.json"
            >>= \eventJson -> parseEvent eventJson @?= Just userUpdatedForTeamHaskell
        ),
      testCase
        "user-created-event"
        ( TIO.readFile "data/system-hooks/user-created.json"
            >>= \eventJson -> parseEvent eventJson @?= Just userCreatedHaskell
        ),
      testCase
        "user-removed-event"
        ( TIO.readFile "data/system-hooks/user-removed.json"
            >>= \eventJson -> parseEvent eventJson @?= Just userRemovedHaskell
        ),
      testCase
        "user-failed-login-event"
        ( TIO.readFile "data/system-hooks/user-failed-login.json"
            >>= \eventJson -> parseEvent eventJson @?= Just userFailedLoginHaskell
        ),
      testCase
        "user-renamed-event"
        ( TIO.readFile "data/system-hooks/user-renamed.json"
            >>= \eventJson -> parseEvent eventJson @?= Just userRenamedHaskell
        ),
      testCase
        "key-added-event"
        ( TIO.readFile "data/system-hooks/key-added.json"
            >>= \eventJson -> parseEvent eventJson @?= Just keyCreatedHaskell
        ),
      testCase
        "key-removed-event"
        ( TIO.readFile "data/system-hooks/key-removed.json"
            >>= \eventJson -> parseEvent eventJson @?= Just keyRemovedHaskell
        ),
      testCase
        "group-created-event"
        ( TIO.readFile "data/system-hooks/group-created.json"
            >>= \eventJson -> parseEvent eventJson @?= Just groupCreatedHaskell
        ),
      testCase
        "group-removed-event"
        ( TIO.readFile "data/system-hooks/group-removed.json"
            >>= \eventJson -> parseEvent eventJson @?= Just groupRemovedHaskell
        ),
      testCase
        "group-renamed-event"
        ( TIO.readFile "data/system-hooks/group-renamed.json"
            >>= \eventJson -> parseEvent eventJson @?= Just groupRenamedHaskell
        ),
      testCase
        "new-group-member-event"
        ( TIO.readFile "data/system-hooks/new-group-member.json"
            >>= \eventJson -> parseEvent eventJson @?= Just newGroupMemberHaskell
        ),
      testCase
        "group-member-removed-event"
        ( TIO.readFile "data/system-hooks/group-member-removed.json"
            >>= \eventJson -> parseEvent eventJson @?= Just groupMemberRemovedHaskell
        ),
      testCase
        "group-member-updated-event"
        ( TIO.readFile "data/system-hooks/group-member-updated.json"
            >>= \eventJson -> parseEvent eventJson @?= Just groupMemberUpdatedHaskell
        ),
      testCase
        "push-event"
        ( TIO.readFile "data/system-hooks/push.json"
            >>= \eventJson -> parseEvent eventJson @?= Just pushHaskell
        ),
      testCase
        "tag-push-event"
        ( TIO.readFile "data/system-hooks/tag-push.json"
            >>= \eventJson -> parseEvent eventJson @?= Just tagPushHaskell
        ),
      testCase
        "merge-request-event"
        ( TIO.readFile "data/system-hooks/merge-request.json"
            >>= \eventJson -> parseEvent eventJson @?= Just mergeRequestHaskell
        ),
      testCase
        "merge-request-event-hw-october-2022"
        ( TIO.readFile "data/system-hooks/merge-request-gitlab-15.5.0.json"
            >>= \eventJson -> parseEvent eventJson @?= Just mergeRequestGitLab_15_5_0_Haskell
        ),
      testCase
        "merge-request-event-hw-maybe-descriptions"
        ( TIO.readFile "data/system-hooks/merge-request-haskell-gitlab-d1ca1037944616ac940284c5f8e49b5d9bcbf83c.json"
            >>= \eventJson -> parseEvent eventJson @?= Just mergeRequestGitLab_15_5_0_maybe_descriptions_Haskell
        ),
      testCase
        "repository-update-event"
        ( TIO.readFile "data/system-hooks/repository-update.json"
            >>= \eventJson -> parseEvent eventJson @?= Just repositoryUpdateHaskell
        ),
      testCase
        "build1"
        ( TIO.readFile "data/system-hooks/build.json"
            >>= \eventJson -> parseEvent eventJson @?= Just buildHaskell
        ),
      testCase
        "build2"
        ( TIO.readFile "data/system-hooks/build2.json"
            >>= \eventJson -> parseEvent eventJson @?= Just build2Haskell
        ),
      testCase
        "pipeline1"
        ( TIO.readFile "data/system-hooks/pipeline.json"
            >>= \eventJson -> parseEvent eventJson @?= Just pipelineHaskell
        ),
      testCase
        "pipeline2"
        ( TIO.readFile "data/system-hooks/pipeline2.json"
            >>= \eventJson -> parseEvent eventJson @?= Just pipeline2Haskell
        )
    ]

matchTest :: String -> String -> Rule -> String -> Rule -> [TestTree]
matchTest lbl jsonFilename rule wrongJson wrongRule =
  [ testCase lbl $
      runGitLabDbg
        ( liftIO (TIO.readFile ("data/system-hooks/" <> jsonFilename))
            >>= \eventJson -> tryFire eventJson rule
        )
        @? (lbl <> " failed"),
    testCase (lbl <> "-wrong-json") $
      not
        <$> runGitLabDbg
          ( liftIO (TIO.readFile ("data/system-hooks/" <> wrongJson))
              >>= \eventJson -> tryFire eventJson rule
          )
        @? (lbl <> "-wrong-json failed"),
    testCase (lbl <> "-wrong-rule") $
      not
        <$> runGitLabDbg
          ( liftIO (TIO.readFile ("data/system-hooks/" <> jsonFilename))
              >>= \eventJson -> tryFire eventJson wrongRule
          )
        @? (lbl <> "-wrong-rule failed")
  ]

matchIfTest :: String -> String -> Rule -> Rule -> [TestTree]
matchIfTest lbl jsonFilename yesFire noFire =
  [ testCase (lbl <> "-yes") $
      runGitLabDbg
        ( liftIO (TIO.readFile ("data/system-hooks/" <> jsonFilename))
            >>= \eventJson -> tryFire eventJson yesFire
        )
        @? (lbl <> "-fireIf-yes failed"),
    testCase (lbl <> "-no") $
      not
        <$> runGitLabDbg
          ( liftIO (TIO.readFile ("data/system-hooks/" <> jsonFilename))
              >>= \eventJson -> tryFire eventJson noFire
          )
        @? (lbl <> "-fireIf-no failed")
  ]

-- [ testCase lbl $
--     runGitLabDbg
--       ( liftIO (TIO.readFile ("data/system-hooks/" <> jsonFilename))
--           >>= \eventJson -> fire eventJson rule
--       )
--       @? (lbl <> " failed"),
--   testCase (lbl <> "-wrong-json") $
--     not
--       <$> runGitLabDbg
--         ( liftIO (TIO.readFile ("data/system-hooks/" <> wrongJson))
--             >>= \eventJson -> fire eventJson rule
--         )
--       @? (lbl <> "-wrong-json failed"),
--   testCase (lbl <> "-wrong-rule") $
--     not
--       <$> runGitLabDbg
--         ( liftIO (TIO.readFile ("data/system-hooks/" <> jsonFilename))
--             >>= \eventJson -> fire eventJson wrongRule
--         )
--       @? (lbl <> "-wrong-rule failed")
-- ]

matchTests :: TestTree
matchTests =
  testGroup "GitLab system hook match" $
    matchTest "project-create" "project-created.json" projectCreateRule "project-destroyed.json" projectDestroyRule
      <> matchTest "project-destroy" "project-destroyed.json" projectDestroyRule "project-created.json" projectCreateRule
      <> matchTest "project-rename" "project-renamed.json" projectRenameRule "project-created.json" projectCreateRule
      <> matchTest "project-transfer" "project-transferred.json" projectTransferRule "project-created.json" projectCreateRule
      <> matchTest "project-update" "project-updated.json" projectUpdateRule "project-created.json" projectCreateRule
      <> matchTest "user-add-to-team" "new-team-member.json" userAddToTeamRule "project-created.json" projectCreateRule
      <> matchTest "user-update-for-team" "team-member-updated.json" userUpdateForTeamRule "project-created.json" projectCreateRule
      <> matchTest "user-remove-from-team" "team-member-removed.json" userRemoveFromTeamRule "project-created.json" projectCreateRule
      <> matchTest "user-create" "user-created.json" userCreateRule "project-created.json" projectCreateRule
      <> matchTest "user-remove" "user-removed.json" userRemoveRule "project-created.json" projectCreateRule
      <> matchTest "user-failed-login" "user-failed-login.json" userFailedLoginRule "project-created.json" projectCreateRule
      <> matchTest "user-rename" "user-renamed.json" userRenameRule "project-created.json" projectCreateRule
      <> matchTest "key-create" "key-added.json" keyCreateRule "project-created.json" projectCreateRule
      <> matchTest "key-remove" "key-removed.json" keyRemoveRule "project-created.json" projectCreateRule
      <> matchTest "group-create" "group-created.json" groupCreateRule "project-created.json" projectCreateRule
      <> matchTest "group-remove" "group-removed.json" groupRemoveRule "project-created.json" projectCreateRule
      <> matchTest "group-rename" "group-renamed.json" groupRenameRule "project-created.json" projectCreateRule
      <> matchTest "new-group-member" "new-group-member.json" newGroupMemberRule "project-created.json" projectCreateRule
      <> matchTest "group-member-remove" "group-member-removed.json" groupMemberRemoveRule "project-created.json" projectCreateRule
      <> matchTest "group-member-update" "group-member-updated.json" groupMemberUpdateRule "project-created.json" projectCreateRule
      <> matchTest "push" "push.json" pushRule "project-created.json" projectCreateRule
      <> matchTest "tag-push" "tag-push.json" tagPushRule "project-created.json" projectCreateRule
      <> matchTest "repository-update" "repository-update.json" repositoryUpdateRule "project-created.json" projectCreateRule
      <> matchTest "merge-request" "merge-request.json" mergeRequestRule "project-created.json" projectCreateRule

matchIfTests :: TestTree
matchIfTests =
  testGroup "GitLab system hook matchIf" $
    matchIfTest "project-create" "project-created.json" projectCreateIfRuleYes projectCreateIfRuleNo
      <> matchIfTest "project-destroy" "project-destroyed.json" projectDestroyIfRuleYes projectDestroyIfRuleNo
      <> matchIfTest "project-rename" "project-renamed.json" projectRenameIfRuleYes projectRenameIfRuleNo
      <> matchIfTest "project-transfer" "project-transferred.json" projectTransferIfRuleYes projectTransferIfRuleNo
      <> matchIfTest "project-update" "project-updated.json" projectUpdateIfRuleYes projectUpdateIfRuleNo
      <> matchIfTest "user-add-to-team" "new-team-member.json" userAddToTeamIfRuleYes userAddToTeamIfRuleNo
      <> matchIfTest "user-update-for-team" "team-member-updated.json" userUpdateForTeamIfRuleYes userUpdateForTeamIfRuleNo
      <> matchIfTest "user-remove-from-team" "team-member-removed.json" userRemoveFromTeamIfRuleYes userRemoveFromTeamIfRuleNo
      <> matchIfTest "user-create" "user-created.json" userCreateIfRuleYes userCreateIfRuleNo
      <> matchIfTest "user-remove" "user-removed.json" userRemoveIfRuleYes userRemoveIfRuleNo
      <> matchIfTest "user-failed-login" "user-failed-login.json" userFailedLoginIfRuleYes userFailedLoginIfRuleNo
      <> matchIfTest "user-rename" "user-renamed.json" userRenameIfRuleYes userRenameIfRuleNo
      <> matchIfTest "key-create" "key-added.json" keyCreateIfRuleYes keyCreateIfRuleNo
      <> matchIfTest "key-remove" "key-removed.json" keyRemoveIfRuleYes keyRemoveIfRuleNo
      <> matchIfTest "group-create" "group-created.json" groupCreateIfRuleYes groupCreateIfRuleNo
      <> matchIfTest "group-remove" "group-removed.json" groupRemoveIfRuleYes groupRemoveIfRuleNo
      <> matchIfTest "group-rename" "group-renamed.json" groupRenameIfRuleYes groupRenameIfRuleNo
      <> matchIfTest "new-group-member" "new-group-member.json" newGroupMemberIfRuleYes newGroupMemberIfRuleNo
      <> matchIfTest "group-member-remove" "group-member-removed.json" groupMemberRemoveIfRuleYes groupMemberRemoveIfRuleNo
      <> matchIfTest "group-member-update" "group-member-updated.json" groupMemberUpdateIfRuleYes groupMemberUpdateIfRuleNo
      <> matchIfTest "push" "push.json" pushIfRuleYes pushIfRuleNo
      <> matchIfTest "tag-push" "tag-push.json" tagPushIfRuleYes tagPushIfRuleNo
      <> matchIfTest "repository-update" "repository-update.json" repositoryUpdateIfRuleYes repositoryUpdateIfRuleNo
      <> matchIfTest "merge-request" "merge-request.json" mergeRequestIfRuleYes mergeRequestIfRuleNo

receiveTests :: TestTree
receiveTests =
  testGroup
    "GitLab system hooks receive"
    [ testCase "1-rule-match" $
        runGitLabDbg $
          liftIO (TIO.readFile "data/system-hooks/project-created.json")
            >>= \eventJson ->
              receiveString
                eventJson
                [projectCreateRule],
      testCase "1-rule-no-match" $
        runGitLabDbg $
          liftIO (TIO.readFile "data/system-hooks/project-created.json")
            >>= \eventJson ->
              receiveString
                eventJson
                [projectRenameRule],
      testCase
        "2-rules"
        $ runGitLabDbg
        $ liftIO (TIO.readFile "data/system-hooks/project-created.json")
          >>= \eventJson ->
            receiveString
              eventJson
              [ projectCreateRule,
                projectDestroyRule
              ]
    ]

projectCreateRule :: Rule
projectCreateRule =
  match
    "projectCreate rule"
    ( \ProjectCreate {} -> do
        return ()
    )

projectCreateIfRuleYes :: Rule
projectCreateIfRuleYes =
  matchIf
    "projectCreate-if rule"
    ( \proj@ProjectCreate {} -> do
        return (projectCreate_owner_email proj == "johnsmith@gmail.com")
    )
    ( \ProjectCreate {} -> do
        return ()
    )

projectCreateIfRuleNo :: Rule
projectCreateIfRuleNo =
  matchIf
    "projectCreate-if rule"
    ( \proj@ProjectCreate {} -> do
        return (projectCreate_owner_email proj == "johnsmith@hotmail.com")
    )
    ( \ProjectCreate {} -> do
        return ()
    )

projectDestroyRule :: Rule
projectDestroyRule =
  match
    "projectDestroy rule"
    ( \ProjectDestroy {} -> do
        return ()
    )

projectDestroyIfRuleYes :: Rule
projectDestroyIfRuleYes =
  matchIf
    "projectDestroy rule-if yes"
    ( \event@ProjectDestroy {} -> do
        return
          ( projectDestroy_owner_email event
              == "johnsmith@gmail.com"
          )
    )
    ( \ProjectDestroy {} -> do
        return ()
    )

projectDestroyIfRuleNo :: Rule
projectDestroyIfRuleNo =
  matchIf
    "projectDestroy rule-if no"
    ( \event@ProjectDestroy {} -> do
        return
          ( projectDestroy_owner_email event
              == "johnsmith@hotmail.com"
          )
    )
    ( \ProjectDestroy {} -> do
        return ()
    )

projectRenameRule :: Rule
projectRenameRule =
  match
    "projectRename rule"
    ( \ProjectRename {} -> do
        return ()
    )

projectRenameIfRuleYes :: Rule
projectRenameIfRuleYes =
  matchIf
    "projectRename rule-if yes"
    ( \event@ProjectRename {} -> do
        return
          ( projectRename_created_at event
              == "2012-07-21T07:30:58Z"
          )
    )
    ( \ProjectRename {} -> do
        return ()
    )

projectRenameIfRuleNo :: Rule
projectRenameIfRuleNo =
  matchIf
    "projectRename rule-if no"
    ( \event@ProjectRename {} -> do
        return
          ( projectRename_created_at event
              == ""
          )
    )
    ( \ProjectRename {} -> do
        return ()
    )

projectTransferRule :: Rule
projectTransferRule =
  match
    "projectTransfer rule"
    ( \ProjectTransfer {} -> do
        return ()
    )

projectTransferIfRuleYes :: Rule
projectTransferIfRuleYes =
  matchIf
    "projectTransfer rule-if yes"
    ( \event@ProjectTransfer {} -> do
        return
          ( projectTransfer_owner_name event
              == "John Smith"
          )
    )
    ( \ProjectTransfer {} -> do
        return ()
    )

projectTransferIfRuleNo :: Rule
projectTransferIfRuleNo =
  matchIf
    "projectTransfer rule-if no"
    ( \event@ProjectTransfer {} -> do
        return
          ( projectTransfer_owner_name event
              == ""
          )
    )
    ( \ProjectTransfer {} -> do
        return ()
    )

projectUpdateRule :: Rule
projectUpdateRule =
  match
    "projectTransfer rule"
    ( \ProjectUpdate {} -> do
        return ()
    )

projectUpdateIfRuleYes :: Rule
projectUpdateIfRuleYes =
  matchIf
    "projectTransfer rule-if yes"
    ( \event@ProjectUpdate {} -> do
        return
          ( projectUpdate_path event
              == "storecloud"
          )
    )
    ( \ProjectUpdate {} -> do
        return ()
    )

projectUpdateIfRuleNo :: Rule
projectUpdateIfRuleNo =
  matchIf
    "projectTransfer rule-if no"
    ( \event@ProjectUpdate {} -> do
        return
          ( projectUpdate_path event
              == ""
          )
    )
    ( \ProjectUpdate {} -> do
        return ()
    )

groupMemberUpdateRule :: Rule
groupMemberUpdateRule =
  match
    "groupMemberUpdate rule"
    ( \GroupMemberUpdate {} -> do
        return ()
    )

groupMemberUpdateIfRuleYes :: Rule
groupMemberUpdateIfRuleYes =
  matchIf
    "groupMemberUpdate rule-if yes"
    ( \event@GroupMemberUpdate {} -> do
        return
          ( groupMemberUpdate_user_email event
              == "johnsmith@gmail.com"
          )
    )
    ( \GroupMemberUpdate {} -> do
        return ()
    )

groupMemberUpdateIfRuleNo :: Rule
groupMemberUpdateIfRuleNo =
  matchIf
    "groupMemberUpdate rule-if no"
    ( \event@GroupMemberUpdate {} -> do
        return
          ( groupMemberUpdate_user_email event
              == "johnsmith@hotmail.com"
          )
    )
    ( \GroupMemberUpdate {} -> do
        return ()
    )

userAddToTeamRule :: Rule
userAddToTeamRule =
  match
    "userAddToTeam rule"
    ( \UserAddToTeam {} -> do
        return ()
    )

userAddToTeamIfRuleYes :: Rule
userAddToTeamIfRuleYes =
  matchIf
    "userAddToTeam rule-if yes"
    ( \event@UserAddToTeam {} -> do
        return
          ( userAddTeam_user_email event
              == "johnsmith@gmail.com"
          )
    )
    ( \UserAddToTeam {} -> do
        return ()
    )

userAddToTeamIfRuleNo :: Rule
userAddToTeamIfRuleNo =
  matchIf
    "userAddToTeam rule-if no"
    ( \event@UserAddToTeam {} ->
        return
          ( userAddTeam_user_email event
              == "johnsmith@hotmail.com"
          )
    )
    ( \UserAddToTeam {} -> do
        return ()
    )

userUpdateForTeamRule :: Rule
userUpdateForTeamRule =
  match
    "userUpdateForTeam rule"
    ( \UserUpdateForTeam {} -> do
        return ()
    )

userUpdateForTeamIfRuleYes :: Rule
userUpdateForTeamIfRuleYes =
  matchIf
    "userUpdateForTeam rule-if yes"
    ( \event@UserUpdateForTeam {} -> do
        return
          ( userUpdateTeam_project_path_with_namespace event
              == "jsmith/storecloud"
          )
    )
    ( \UserUpdateForTeam {} -> do
        return ()
    )

userUpdateForTeamIfRuleNo :: Rule
userUpdateForTeamIfRuleNo =
  matchIf
    "userUpdateForTeam rule-if no"
    ( \event@UserUpdateForTeam {} -> do
        return
          ( userUpdateTeam_project_path_with_namespace event
              == ""
          )
    )
    ( \UserUpdateForTeam {} -> do
        return ()
    )

userRemoveFromTeamRule :: Rule
userRemoveFromTeamRule =
  match
    "userRemoveFromTeam rule"
    ( \UserRemoveFromTeam {} -> do
        return ()
    )

userRemoveFromTeamIfRuleYes :: Rule
userRemoveFromTeamIfRuleYes =
  matchIf
    "userRemoveFromTeam rule-if yes"
    ( \event@UserRemoveFromTeam {} -> do
        return
          ( userRemoveTeam_user_email event
              == "johnsmith@gmail.com"
          )
    )
    ( \UserRemoveFromTeam {} -> do
        return ()
    )

userRemoveFromTeamIfRuleNo :: Rule
userRemoveFromTeamIfRuleNo =
  matchIf
    "userRemoveFromTeam rule-if no"
    ( \event@UserRemoveFromTeam {} -> do
        return
          ( userRemoveTeam_user_email event
              == "js@gmail.com"
          )
    )
    ( \UserRemoveFromTeam {} -> do
        return ()
    )

userCreateRule :: Rule
userCreateRule =
  match
    "userCreate rule"
    ( \UserCreate {} -> do
        return ()
    )

userCreateIfRuleYes :: Rule
userCreateIfRuleYes =
  matchIf
    "userCreate rule-if yes"
    ( \event@UserCreate {} -> do
        return
          ( userCreate_email event
              == "js@gitlabhq.com"
          )
    )
    ( \UserCreate {} -> do
        return ()
    )

userCreateIfRuleNo :: Rule
userCreateIfRuleNo =
  matchIf
    "userCreate rule-if no"
    ( \event@UserCreate {} -> do
        return
          ( userCreate_email event
              == "js@gmail.com"
          )
    )
    ( \UserCreate {} -> do
        return ()
    )

userRemoveRule :: Rule
userRemoveRule =
  match
    "userRemove rule"
    ( \UserRemove {} -> do
        return ()
    )

userRemoveIfRuleYes :: Rule
userRemoveIfRuleYes =
  matchIf
    "userRemove rule-if yes"
    ( \event@UserRemove {} -> do
        return
          ( userRemove_email event
              == "js@gitlabhq.com"
          )
    )
    ( \UserRemove {} -> do
        return ()
    )

userRemoveIfRuleNo :: Rule
userRemoveIfRuleNo =
  matchIf
    "userRemove rule-if no"
    ( \event@UserRemove {} -> do
        return
          ( userRemove_email event
              == "js@gmail.com"
          )
    )
    ( \UserRemove {} -> do
        return ()
    )

userFailedLoginRule :: Rule
userFailedLoginRule =
  match
    "userFailedLogin rule"
    ( \UserFailedLogin {} -> do
        return ()
    )

userFailedLoginIfRuleYes :: Rule
userFailedLoginIfRuleYes =
  matchIf
    "userFailedLogin rule"
    ( \event@UserFailedLogin {} -> do
        return
          ( userFailedLogin_email event
              == "user4@example.com"
          )
    )
    ( \UserFailedLogin {} -> do
        return ()
    )

userFailedLoginIfRuleNo :: Rule
userFailedLoginIfRuleNo =
  matchIf
    "userFailedLogin rule"
    ( \event@UserFailedLogin {} -> do
        return
          ( userFailedLogin_email event
              == "user4@gmail.com"
          )
    )
    ( \UserFailedLogin {} -> do
        return ()
    )

userRenameRule :: Rule
userRenameRule =
  match
    "userRename rule"
    ( \UserRename {} -> do
        return ()
    )

userRenameIfRuleYes :: Rule
userRenameIfRuleYes =
  matchIf
    "userRename rule-if yes"
    ( \event@UserRename {} -> do
        return
          ( userRename_username event
              == "new-exciting-name"
          )
    )
    ( \UserRename {} -> do
        return ()
    )

userRenameIfRuleNo :: Rule
userRenameIfRuleNo =
  matchIf
    "userRename rule-if no"
    ( \event@UserRename {} -> do
        return
          ( userRename_username event
              == "old-boring-name"
          )
    )
    ( \UserRename {} -> do
        return ()
    )

keyCreateRule :: Rule
keyCreateRule =
  match
    "keyCreate rule"
    ( \KeyCreate {} -> do
        return ()
    )

keyCreateIfRuleYes :: Rule
keyCreateIfRuleYes =
  matchIf
    "keyCreate rule-if yes"
    ( \event@KeyCreate {} -> do
        return
          ( keyCreate_updated_at event
              == "2012-07-21T07:38:22Z"
          )
    )
    ( \KeyCreate {} -> do
        return ()
    )

keyCreateIfRuleNo :: Rule
keyCreateIfRuleNo =
  matchIf
    "keyCreate rule-if no"
    ( \event@KeyCreate {} -> do
        return
          ( keyCreate_updated_at event
              == ""
          )
    )
    ( \KeyCreate {} -> do
        return ()
    )

keyRemoveRule :: Rule
keyRemoveRule =
  match
    "keyRemove rule"
    ( \KeyRemove {} -> do
        return ()
    )

keyRemoveIfRuleYes :: Rule
keyRemoveIfRuleYes =
  matchIf
    "keyRemove rule-if yes"
    ( \event@KeyRemove {} -> do
        return
          ( keyRemove_updated_at event
              == "2012-07-21T07:38:22Z"
          )
    )
    ( \KeyRemove {} -> do
        return ()
    )

keyRemoveIfRuleNo :: Rule
keyRemoveIfRuleNo =
  matchIf
    "keyRemove rule-if no"
    ( \event@KeyRemove {} -> do
        return
          ( keyRemove_updated_at event
              == ""
          )
    )
    ( \KeyRemove {} -> do
        return ()
    )

groupCreateRule :: Rule
groupCreateRule =
  match
    "groupCreate rule"
    ( \GroupCreate {} -> do
        return ()
    )

groupCreateIfRuleYes :: Rule
groupCreateIfRuleYes =
  matchIf
    "groupCreate rule-if yes"
    ( \event@GroupCreate {} -> do
        return
          ( groupCreate_name event
              == "StoreCloud"
          )
    )
    ( \GroupCreate {} -> do
        return ()
    )

groupCreateIfRuleNo :: Rule
groupCreateIfRuleNo =
  matchIf
    "groupCreate rule-if no"
    ( \event@GroupCreate {} -> do
        return
          ( groupCreate_name event
              == ""
          )
    )
    ( \GroupCreate {} -> do
        return ()
    )

groupRemoveRule :: Rule
groupRemoveRule =
  match
    "groupRemove rule"
    ( \GroupRemove {} -> do
        return ()
    )

groupRemoveIfRuleYes :: Rule
groupRemoveIfRuleYes =
  matchIf
    "groupRemove rule-if yes"
    ( \event@GroupRemove {} -> do
        return
          ( groupRemove_name event
              == "StoreCloud"
          )
    )
    ( \GroupRemove {} -> do
        return ()
    )

groupRemoveIfRuleNo :: Rule
groupRemoveIfRuleNo =
  matchIf
    "groupRemove rule-if no"
    ( \event@GroupRemove {} -> do
        return
          ( groupRemove_name event
              == ""
          )
    )
    ( \GroupRemove {} -> do
        return ()
    )

groupRenameRule :: Rule
groupRenameRule =
  match
    "groupRename rule"
    ( \GroupRename {} -> do
        return ()
    )

groupRenameIfRuleYes :: Rule
groupRenameIfRuleYes =
  matchIf
    "groupRename rule-if yes"
    ( \event@GroupRename {} -> do
        return
          ( groupRename_full_path event
              == "parent-group/better-name"
          )
    )
    ( \GroupRename {} -> do
        return ()
    )

groupRenameIfRuleNo :: Rule
groupRenameIfRuleNo =
  matchIf
    "groupRename rule-if no"
    ( \event@GroupRename {} -> do
        return
          ( groupRename_full_path event
              == "parent-group/wrong-name"
          )
    )
    ( \GroupRename {} -> do
        return ()
    )

newGroupMemberRule :: Rule
newGroupMemberRule =
  match
    "newGroupMember rule"
    ( \NewGroupMember {} -> do
        return ()
    )

newGroupMemberIfRuleYes :: Rule
newGroupMemberIfRuleYes =
  matchIf
    "newGroupMember rule-if yes"
    ( \event@NewGroupMember {} -> do
        return
          ( newGroupMember_user_email event
              == "johnsmith@gmail.com"
          )
    )
    ( \NewGroupMember {} -> do
        return ()
    )

newGroupMemberIfRuleNo :: Rule
newGroupMemberIfRuleNo =
  matchIf
    "newGroupMember rule-if no"
    ( \event@NewGroupMember {} -> do
        return
          ( newGroupMember_user_email event
              == "johnsmith@hotmail.com"
          )
    )
    ( \NewGroupMember {} -> do
        return ()
    )

groupMemberRemoveRule :: Rule
groupMemberRemoveRule =
  match
    "groupMemberRemove rule"
    ( \GroupMemberRemove {} -> do
        return ()
    )

groupMemberRemoveIfRuleYes :: Rule
groupMemberRemoveIfRuleYes =
  matchIf
    "groupMemberRemove rule-if yes"
    ( \event@GroupMemberRemove {} -> do
        return
          ( groupMemberRemove_user_email event
              == "johnsmith@gmail.com"
          )
    )
    ( \GroupMemberRemove {} -> do
        return ()
    )

groupMemberRemoveIfRuleNo :: Rule
groupMemberRemoveIfRuleNo =
  matchIf
    "groupMemberRemove rule-if no"
    ( \event@GroupMemberRemove {} -> do
        return
          ( groupMemberRemove_user_email event
              == "johnsmith@hotmail.com"
          )
    )
    ( \GroupMemberRemove {} -> do
        return ()
    )

pushRule :: Rule
pushRule =
  match
    "push rule"
    ( \Push {} -> do
        return ()
    )

pushIfRuleYes :: Rule
pushIfRuleYes =
  matchIf
    "push rule-if yes"
    ( \event@Push {} -> do
        return (push_checkout_sha event == "da1560886d4f094c3e6c9ef40349f7d38b5d27d7")
    )
    ( \Push {} -> do
        return ()
    )

pushIfRuleNo :: Rule
pushIfRuleNo =
  matchIf
    "push rule-if no"
    ( \event@Push {} -> do
        return (push_checkout_sha event == "")
    )
    ( \Push {} -> do
        return ()
    )

tagPushRule :: Rule
tagPushRule =
  match
    "tagPush rule"
    ( \TagPush {} -> do
        return ()
    )

tagPushIfRuleYes :: Rule
tagPushIfRuleYes =
  matchIf
    "tagPush rule-if yes"
    ( \event@TagPush {} -> do
        return (tagPush_ref event == "refs/tags/v1.0.0")
    )
    ( \TagPush {} -> do
        return ()
    )

tagPushIfRuleNo :: Rule
tagPushIfRuleNo =
  matchIf
    "tagPush rule-if no"
    ( \event@TagPush {} -> do
        return (tagPush_ref event == "refs/tags/v2.0.0")
    )
    ( \TagPush {} -> do
        return ()
    )

repositoryUpdateRule :: Rule
repositoryUpdateRule =
  match
    "repositoryUpdate rule"
    ( \RepositoryUpdate {} -> do
        return ()
    )

repositoryUpdateIfRuleYes :: Rule
repositoryUpdateIfRuleYes =
  matchIf
    "repositoryUpdate rule-if yes"
    ( \event@RepositoryUpdate {} -> do
        return (repositoryUpdate_refs event == ["refs/heads/master"])
    )
    ( \RepositoryUpdate {} -> do
        return ()
    )

repositoryUpdateIfRuleNo :: Rule
repositoryUpdateIfRuleNo =
  matchIf
    "repositoryUpdate rule-if no"
    ( \event@RepositoryUpdate {} -> do
        return (repositoryUpdate_refs event == ["refs/heads/branch-1"])
    )
    ( \RepositoryUpdate {} -> do
        return ()
    )

mergeRequestRule :: Rule
mergeRequestRule =
  match
    "mergeRequest rule"
    ( \MergeRequestEvent {} -> do
        return ()
    )

mergeRequestIfRuleYes :: Rule
mergeRequestIfRuleYes =
  matchIf
    "mergeRequest rule-if yes"
    ( \event@MergeRequestEvent {} -> do
        return (userEvent_name (mergeRequest_user event) == "Administrator")
    )
    ( \MergeRequestEvent {} -> do
        return ()
    )

mergeRequestIfRuleNo :: Rule
mergeRequestIfRuleNo =
  matchIf
    "mergeRequest rule-if no"
    ( \event@MergeRequestEvent {} -> do
        return (userEvent_name (mergeRequest_user event) == "joe")
    )
    ( \MergeRequestEvent {} -> do
        return ()
    )

projectCreatedHaskell :: ProjectCreate
projectCreatedHaskell =
  ProjectCreate
    { projectCreate_created_at = "2012-07-21T07:30:54Z",
      projectCreate_updated_at = "2012-07-21T07:38:22Z",
      projectCreate_action = "project_create",
      projectCreate_name = "StoreCloud",
      projectCreate_owner_email = "johnsmith@gmail.com",
      projectCreate_owner_name = "John Smith",
      projectCreate_path = "storecloud",
      projectCreate_path_with_namespace = "jsmith/storecloud",
      projectCreate_project_id = 74,
      projectCreate_project_visibility = Private
    }

projectCreatedDiatricsHaskell :: ProjectCreate
projectCreatedDiatricsHaskell =
  ProjectCreate
    { projectCreate_created_at = "2012-07-21T07:30:54Z",
      projectCreate_updated_at = "2012-07-21T07:38:22Z",
      projectCreate_action = "project_create",
      projectCreate_name = "StoreCloud",
      projectCreate_owner_email = "eloisesmith@gmail.com",
      projectCreate_owner_name = "Smith, Eloïse",
      projectCreate_path = "storecloud",
      projectCreate_path_with_namespace = "eloisesmith/storecloud",
      projectCreate_project_id = 74,
      projectCreate_project_visibility = Private
    }

projectDestroyedHaskell :: ProjectDestroy
projectDestroyedHaskell =
  ProjectDestroy
    { projectDestroy_created_at = "2012-07-21T07:30:58Z",
      projectDestroy_updated_at = "2012-07-21T07:38:22Z",
      projectDestroy_action = "project_destroy",
      projectDestroy_name = "Underscore",
      projectDestroy_owner_email = "johnsmith@gmail.com",
      projectDestroy_owner_name = "John Smith",
      projectDestroy_path = "underscore",
      projectDestroy_path_with_namespace = "jsmith/underscore",
      projectDestroy_project_id = 73,
      projectDestroy_project_visibility = Internal
    }

projectRenamedHaskell :: ProjectRename
projectRenamedHaskell =
  ProjectRename
    { projectRename_created_at = "2012-07-21T07:30:58Z",
      projectRename_updated_at = "2012-07-21T07:38:22Z",
      projectRename_event_name = "project_rename",
      projectRename_name = "Underscore",
      projectRename_path = "underscore",
      projectRename_path_with_namespace = "jsmith/underscore",
      projectRename_project_id = 73,
      projectRename_owner_name = "John Smith",
      projectRename_owner_email = "johnsmith@gmail.com",
      projectRename_project_visibility = Internal,
      projectRename_old_path_with_namespace = "jsmith/overscore"
    }

projectTransferredHaskell :: ProjectTransfer
projectTransferredHaskell =
  ProjectTransfer
    { projectTransfer_created_at = "2012-07-21T07:30:58Z",
      projectTransfer_updated_at = "2012-07-21T07:38:22Z",
      projectTransfer_event_name = "project_transfer",
      projectTransfer_name = "Underscore",
      projectTransfer_path = "underscore",
      projectTransfer_path_with_namespace = "scores/underscore",
      projectTransfer_project_id = 73,
      projectTransfer_owner_name = "John Smith",
      projectTransfer_owner_email = "johnsmith@gmail.com",
      projectTransfer_project_visibility = Internal,
      projectTransfer_old_path_with_namespace = "jsmith/overscore"
    }

projectUpdatedHaskell :: ProjectUpdate
projectUpdatedHaskell =
  ProjectUpdate
    { projectUpdate_created_at = "2012-07-21T07:30:54Z",
      projectUpdate_updated_at = "2012-07-21T07:38:22Z",
      projectUpdate_event_name = "project_update",
      projectUpdate_name = "StoreCloud",
      projectUpdate_owner_email = "johnsmith@gmail.com",
      projectUpdate_owner_name = "John Smith",
      projectUpdate_path = "storecloud",
      projectUpdate_path_with_namespace = "jsmith/storecloud",
      projectUpdate_project_id = 74,
      projectUpdate_project_visibility = Private
    }

userAddedToTeamHaskell :: UserAddToTeam
userAddedToTeamHaskell =
  UserAddToTeam
    { userAddTeam_created_at = "2012-07-21T07:30:56Z",
      userAddTeam_updated_at = "2012-07-21T07:38:22Z",
      userAddTeam_event_name = "user_add_to_team",
      userAddTeam_access_level = "Maintainer",
      userAddTeam_project_id = 74,
      userAddTeam_project_name = "StoreCloud",
      userAddTeam_project_path = "storecloud",
      userAddTeam_project_path_with_namespace = "jsmith/storecloud",
      userAddTeam_user_email = "johnsmith@gmail.com",
      userAddTeam_user_name = "John Smith",
      userAddTeam_user_username = "johnsmith",
      userAddTeam_user_id = 41,
      userAddTeam_project_visibility = Private
    }

userUpdatedForTeamHaskell :: UserUpdateForTeam
userUpdatedForTeamHaskell =
  UserUpdateForTeam
    { userUpdateTeam_created_at = "2012-07-21T07:30:56Z",
      userUpdateTeam_updated_at = "2012-07-21T07:38:22Z",
      userUpdateTeam_event_name = "user_update_for_team",
      userUpdateTeam_access_level = "Maintainer",
      userUpdateTeam_project_id = 74,
      userUpdateTeam_project_name = "StoreCloud",
      userUpdateTeam_project_path = "storecloud",
      userUpdateTeam_project_path_with_namespace = "jsmith/storecloud",
      userUpdateTeam_user_email = "johnsmith@gmail.com",
      userUpdateTeam_user_name = "John Smith",
      userUpdateTeam_user_username = "johnsmith",
      userUpdateTeam_user_id = 41,
      userUpdateTeam_project_visibility = Private
    }

userRemovedFromTeamHaskell :: UserRemoveFromTeam
userRemovedFromTeamHaskell =
  UserRemoveFromTeam
    { userRemoveTeam_created_at = "2012-07-21T07:30:56Z",
      userRemoveTeam_updated_at = "2012-07-21T07:38:22Z",
      userRemoveTeam_event_name = "user_remove_from_team",
      userRemoveTeam_access_level = "Maintainer",
      userRemoveTeam_project_id = 74,
      userRemoveTeam_project_name = "StoreCloud",
      userRemoveTeam_project_path = "storecloud",
      userRemoveTeam_project_path_with_namespace = "jsmith/storecloud",
      userRemoveTeam_user_email = "johnsmith@gmail.com",
      userRemoveTeam_user_name = "John Smith",
      userRemoveTeam_user_username = "johnsmith",
      userRemoveTeam_user_id = 41,
      userRemoveTeam_project_visibility = Private
    }

userCreatedHaskell :: UserCreate
userCreatedHaskell =
  UserCreate
    { userCreate_created_at = "2012-07-21T07:44:07Z",
      userCreate_updated_at = "2012-07-21T07:38:22Z",
      userCreate_email = "js@gitlabhq.com",
      userCreate_event_name = "user_create",
      userCreate_name = "John Smith",
      userCreate_username = "js",
      userCreate_user_id = 41
    }

userRemovedHaskell :: UserRemove
userRemovedHaskell =
  UserRemove
    { userRemove_created_at = "2012-07-21T07:44:07Z",
      userRemove_updated_at = "2012-07-21T07:38:22Z",
      userRemove_email = "js@gitlabhq.com",
      userRemove_event_name = "user_destroy",
      userRemove_name = "John Smith",
      userRemove_username = "js",
      userRemove_user_id = 41
    }

userFailedLoginHaskell :: UserFailedLogin
userFailedLoginHaskell =
  UserFailedLogin
    { userFailedLogin_event_name = "user_failed_login",
      userFailedLogin_created_at = "2017-10-03T06:08:48Z",
      userFailedLogin_updated_at = "2018-01-15T04:52:06Z",
      userFailedLogin_name = "John Smith",
      userFailedLogin_email = "user4@example.com",
      userFailedLogin_user_id = 26,
      userFailedLogin_username = "user4",
      userFailedLogin_state = "blocked"
    }

userRenamedHaskell :: UserRename
userRenamedHaskell =
  UserRename
    { userRename_event_name = "user_rename",
      userRename_created_at = "2017-11-01T11:21:04Z",
      userRename_updated_at = "2017-11-01T14:04:47Z",
      userRename_name = "new-name",
      userRename_email = "best-email@example.tld",
      userRename_user_id = 58,
      userRename_username = "new-exciting-name",
      userRename_old_username = "old-boring-name"
    }

keyCreatedHaskell :: KeyCreate
keyCreatedHaskell =
  KeyCreate
    { keyCreate_event_name = "key_create",
      keyCreate_created_at = "2014-08-18 18:45:16 UTC",
      keyCreate_updated_at = "2012-07-21T07:38:22Z",
      keyCreate_username = "root",
      keyCreate_key = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC58FwqHUbebw2SdT7SP4FxZ0w+lAO/erhy2ylhlcW/tZ3GY3mBu9VeeiSGoGz8hCx80Zrz+aQv28xfFfKlC8XQFpCWwsnWnQqO2Lv9bS8V1fIHgMxOHIt5Vs+9CAWGCCvUOAurjsUDoE2ALIXLDMKnJxcxD13XjWdK54j6ZXDB4syLF0C2PnAQSVY9X7MfCYwtuFmhQhKaBussAXpaVMRHltie3UYSBUUuZaB3J4cg/7TxlmxcNd+ppPRIpSZAB0NI6aOnqoBCpimscO/VpQRJMVLr3XiSYeT6HBiDXWHnIVPfQc03OGcaFqOit6p8lYKMaP/iUQLm+pgpZqrXZ9vB john@localhost",
      keyCreate_id = 4
    }

keyRemovedHaskell :: KeyRemove
keyRemovedHaskell =
  KeyRemove
    { keyRemove_event_name = "key_destroy",
      keyRemove_created_at = "2014-08-18 18:45:16 UTC",
      keyRemove_updated_at = "2012-07-21T07:38:22Z",
      keyRemove_username = "root",
      keyRemove_key = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC58FwqHUbebw2SdT7SP4FxZ0w+lAO/erhy2ylhlcW/tZ3GY3mBu9VeeiSGoGz8hCx80Zrz+aQv28xfFfKlC8XQFpCWwsnWnQqO2Lv9bS8V1fIHgMxOHIt5Vs+9CAWGCCvUOAurjsUDoE2ALIXLDMKnJxcxD13XjWdK54j6ZXDB4syLF0C2PnAQSVY9X7MfCYwtuFmhQhKaBussAXpaVMRHltie3UYSBUUuZaB3J4cg/7TxlmxcNd+ppPRIpSZAB0NI6aOnqoBCpimscO/VpQRJMVLr3XiSYeT6HBiDXWHnIVPfQc03OGcaFqOit6p8lYKMaP/iUQLm+pgpZqrXZ9vB john@localhost",
      keyRemove_id = 4
    }

groupCreatedHaskell :: GroupCreate
groupCreatedHaskell =
  GroupCreate
    { groupCreate_created_at = "2012-07-21T07:30:54Z",
      groupCreate_updated_at = "2012-07-21T07:38:22Z",
      groupCreate_event_name = "group_create",
      groupCreate_name = "StoreCloud",
      groupCreate_owner_email = Nothing,
      groupCreate_owner_name = Nothing,
      groupCreate_path = "storecloud",
      groupCreate_group_id = 78
    }

groupRemovedHaskell :: GroupRemove
groupRemovedHaskell =
  GroupRemove
    { groupRemove_created_at = "2012-07-21T07:30:54Z",
      groupRemove_updated_at = "2012-07-21T07:38:22Z",
      groupRemove_event_name = "group_destroy",
      groupRemove_name = "StoreCloud",
      groupRemove_owner_email = Nothing,
      groupRemove_owner_name = Nothing,
      groupRemove_path = "storecloud",
      groupRemove_group_id = 78
    }

groupRenamedHaskell :: GroupRename
groupRenamedHaskell =
  GroupRename
    { groupRename_event_name = "group_rename",
      groupRename_created_at = "2017-10-30T15:09:00Z",
      groupRename_updated_at = "2017-11-01T10:23:52Z",
      groupRename_name = "Better Name",
      groupRename_path = "better-name",
      groupRename_full_path = "parent-group/better-name",
      groupRename_group_id = 64,
      groupRename_owner_name = Nothing,
      groupRename_owner_email = Nothing,
      groupRename_old_path = "old-name",
      groupRename_old_full_path = "parent-group/old-name"
    }

newGroupMemberHaskell :: NewGroupMember
newGroupMemberHaskell =
  NewGroupMember
    { newGroupMember_created_at = "2012-07-21T07:30:56Z",
      newGroupMember_updated_at = "2012-07-21T07:38:22Z",
      newGroupMember_event_name = "user_add_to_group",
      newGroupMember_group_access = "Maintainer",
      newGroupMember_group_id = 78,
      newGroupMember_group_name = "StoreCloud",
      newGroupMember_group_path = "storecloud",
      newGroupMember_user_email = "johnsmith@gmail.com",
      newGroupMember_user_name = "John Smith",
      newGroupMember_user_username = "johnsmith",
      newGroupMember_user_id = 41
    }

groupMemberRemovedHaskell :: GroupMemberRemove
groupMemberRemovedHaskell =
  GroupMemberRemove
    { groupMemberRemove_created_at = "2012-07-21T07:30:56Z",
      groupMemberRemove_updated_at = "2012-07-21T07:38:22Z",
      groupMemberRemove_event_name = "user_remove_from_group",
      groupMemberRemove_group_access = "Maintainer",
      groupMemberRemove_group_id = 78,
      groupMemberRemove_group_name = "StoreCloud",
      groupMemberRemove_group_path = "storecloud",
      groupMemberRemove_user_email = "johnsmith@gmail.com",
      groupMemberRemove_user_name = "John Smith",
      groupMemberRemove_user_username = "johnsmith",
      groupMemberRemove_user_id = 41
    }

groupMemberUpdatedHaskell :: GroupMemberUpdate
groupMemberUpdatedHaskell =
  GroupMemberUpdate
    { groupMemberUpdate_created_at = "2012-07-21T07:30:56Z",
      groupMemberUpdate_updated_at = "2012-07-21T07:38:22Z",
      groupMemberUpdate_event_name = "user_update_for_group",
      groupMemberUpdate_group_access = "Maintainer",
      groupMemberUpdate_group_id = 78,
      groupMemberUpdate_group_name = "StoreCloud",
      groupMemberUpdate_group_path = "storecloud",
      groupMemberUpdate_user_email = "johnsmith@gmail.com",
      groupMemberUpdate_user_name = "John Smith",
      groupMemberUpdate_user_username = "johnsmith",
      groupMemberUpdate_user_id = 41
    }

pushHaskell :: Push
pushHaskell =
  Push
    { push_event_name = "push",
      push_before = "95790bf891e76fee5e1747ab589903a6a1f80f22",
      push_after = "da1560886d4f094c3e6c9ef40349f7d38b5d27d7",
      push_ref = "refs/heads/master",
      push_checkout_sha = "da1560886d4f094c3e6c9ef40349f7d38b5d27d7",
      push_user_id = 4,
      push_user_name = "John Smith",
      push_user_email = Just "john@example.com",
      push_user_username = Just "abc1",
      push_user_avatar = "https://s.gravatar.com/avatar/d4c74594d841139328695756648b6bd6?s=8://s.gravatar.com/avatar/d4c74594d841139328695756648b6bd6?s=80",
      push_project_id = 15,
      push_project = ProjectEvent {projectEvent_name = "Diaspora", projectEvent_description = Just "", projectEvent_web_url = "http://example.com/mike/diaspora", projectEvent_avatar_url = Nothing, projectEvent_git_ssh_url = "git@example.com:mike/diaspora.git", projectEvent_git_http_url = "http://example.com/mike/diaspora.git", projectEvent_namespace = "Mike", projectEvent_visibility_level = Private, projectEvent_path_with_namespace = "mike/diaspora", projectEvent_default_branch = "master", projectEvent_homepage = Just "http://example.com/mike/diaspora", projectEvent_url = "git@example.com:mike/diaspora.git", projectEvent_ssh_url = "git@example.com:mike/diaspora.git", projectEvent_http_url = "http://example.com/mike/diaspora.git"},
      push_repository = RepositoryEvent {repositoryEvent_name = "Diaspora", repositoryEvent_url = "git@example.com:mike/diaspora.git", repositoryEvent_description = Just "", repositoryEvent_homepage = Just "http://example.com/mike/diaspora", repositoryEvent_git_http_url = Just "http://example.com/mike/diaspora.git", repositoryEvent_git_ssh_url = Just "git@example.com:mike/diaspora.git", repositoryEvent_visibility_level = Just Private},
      push_commits = [CommitEvent {commitEvent_id = "c5feabde2d8cd023215af4d2ceeb7a64839fc428", commitEvent_message = "Add simple search to projects in public area", commitEvent_timestamp = "2013-05-13T18:18:08+00:00", commitEvent_url = "https://dev.gitlab.org/gitlab/gitlabhq/commit/c5feabde2d8cd023215af4d2ceeb7a64839fc428", commitEvent_author = CommitAuthorEvent {commitAuthorEvent_name = "Example User", commitAuthorEvent_email = "user@example.com"}}],
      push_total_commits_count = 1
    }

tagPushHaskell :: TagPush
tagPushHaskell =
  TagPush
    { tagPush_event_name = "tag_push",
      tagPush_before = "0000000000000000000000000000000000000000",
      tagPush_after = "82b3d5ae55f7080f1e6022629cdb57bfae7cccc7",
      tagPush_ref = "refs/tags/v1.0.0",
      tagPush_checkout_sha = "5937ac0a7beb003549fc5fd26fc247adbce4a52e",
      tagPush_user_id = 1,
      tagPush_user_name = "John Smith",
      tagPush_user_avatar = "https://s.gravatar.com/avatar/d4c74594d841139328695756648b6bd6?s=8://s.gravatar.com/avatar/d4c74594d841139328695756648b6bd6?s=80",
      tagPush_project_id = 1,
      tagPush_project = ProjectEvent {projectEvent_name = "Example", projectEvent_description = Just "", projectEvent_web_url = "http://example.com/jsmith/example", projectEvent_avatar_url = Nothing, projectEvent_git_ssh_url = "git@example.com:jsmith/example.git", projectEvent_git_http_url = "http://example.com/jsmith/example.git", projectEvent_namespace = "Jsmith", projectEvent_visibility_level = Private, projectEvent_path_with_namespace = "jsmith/example", projectEvent_default_branch = "master", projectEvent_homepage = Just "http://example.com/jsmith/example", projectEvent_url = "git@example.com:jsmith/example.git", projectEvent_ssh_url = "git@example.com:jsmith/example.git", projectEvent_http_url = "http://example.com/jsmith/example.git"},
      tagPush_repository = RepositoryEvent {repositoryEvent_name = "Example", repositoryEvent_url = "ssh://git@example.com/jsmith/example.git", repositoryEvent_description = Just "", repositoryEvent_homepage = Just "http://example.com/jsmith/example", repositoryEvent_git_http_url = Just "http://example.com/jsmith/example.git", repositoryEvent_git_ssh_url = Just "git@example.com:jsmith/example.git", repositoryEvent_visibility_level = Just Private},
      tagPush_commits = [],
      tagPush_total_commits_count = 0
    }

repositoryUpdateHaskell :: RepositoryUpdate
repositoryUpdateHaskell =
  RepositoryUpdate
    { repositoryUpdate_event_name = "repository_update",
      repositoryUpdate_user_id = 1,
      repositoryUpdate_user_name = "John Smith",
      repositoryUpdate_user_email = "admin@example.com",
      repositoryUpdate_user_avatar = "https://s.gravatar.com/avatar/d4c74594d841139328695756648b6bd6?s=8://s.gravatar.com/avatar/d4c74594d841139328695756648b6bd6?s=80",
      repositoryUpdate_project_id = 1,
      repositoryUpdate_project = ProjectEvent {projectEvent_name = "Example", projectEvent_description = Just "", projectEvent_web_url = "http://example.com/jsmith/example", projectEvent_avatar_url = Nothing, projectEvent_git_ssh_url = "git@example.com:jsmith/example.git", projectEvent_git_http_url = "http://example.com/jsmith/example.git", projectEvent_namespace = "Jsmith", projectEvent_visibility_level = Private, projectEvent_path_with_namespace = "jsmith/example", projectEvent_default_branch = "master", projectEvent_homepage = Just "http://example.com/jsmith/example", projectEvent_url = "git@example.com:jsmith/example.git", projectEvent_ssh_url = "git@example.com:jsmith/example.git", projectEvent_http_url = "http://example.com/jsmith/example.git"},
      repositoryUpdate_changes = [ProjectChanges {projectChanges_before = "8205ea8d81ce0c6b90fbe8280d118cc9fdad6130", projectChanges_after = "4045ea7a3df38697b3730a20fb73c8bed8a3e69e", projectChanges_ref = "refs/heads/master"}],
      repositoryUpdate_refs = ["refs/heads/master"]
    }

mergeRequestHaskell :: MergeRequestEvent
mergeRequestHaskell =
  MergeRequestEvent {mergeRequest_object_kind = "merge_request", mergeRequest_event_type = "merge_request", mergeRequest_user = UserEvent {userEvent_name = "Administrator", userEvent_username = "root", userEvent_avatar_url = "http://www.gravatar.com/avatar/e64c7d89f26bd1972efa854d13d7dd61?s=40&d=identicon"}, mergeRequest_project = ProjectEvent {projectEvent_name = "Gitlab Test", projectEvent_description = Just "Aut reprehenderit ut est.", projectEvent_web_url = "http://example.com/gitlabhq/gitlab-test", projectEvent_avatar_url = Nothing, projectEvent_git_ssh_url = "git@example.com:gitlabhq/gitlab-test.git", projectEvent_git_http_url = "http://example.com/gitlabhq/gitlab-test.git", projectEvent_namespace = "GitlabHQ", projectEvent_visibility_level = Public, projectEvent_path_with_namespace = "gitlabhq/gitlab-test", projectEvent_default_branch = "master", projectEvent_homepage = Just "http://example.com/gitlabhq/gitlab-test", projectEvent_url = "http://example.com/gitlabhq/gitlab-test.git", projectEvent_ssh_url = "git@example.com:gitlabhq/gitlab-test.git", projectEvent_http_url = "http://example.com/gitlabhq/gitlab-test.git"}, mergeRequest_object_attributes = ObjectAttributes {objectAttributes_id = 99, objectAttributes_target_branch = "master", objectAttributes_source_branch = "ms-viewport", objectAttributes_source_project_id = 14, objectAttributes_author_id = Just 51, objectAttributes_assignee_id = Just 6, objectAttributes_assignee_ids = Nothing, objectAttributes_title = "MS-Viewport", objectAttributes_created_at = "2013-12-03T17:23:34Z", objectAttributes_updated_at = "2013-12-03T17:23:34Z", objectAttributes_milestone_id = Nothing, objectAttributes_state = "opened", objectAttributes_state_id = Nothing, objectAttributes_merge_status = "unchecked", objectAttributes_target_project_id = 14, objectAttributes_iid = 1, objectAttributes_description = "", objectAttributes_updated_by_id = Nothing, objectAttributes_merge_error = Nothing, objectAttributes_merge_params = Nothing, objectAttributes_merge_when_pipeline_succeeds = Nothing, objectAttributes_merge_user_id = Nothing, objectAttributes_merge_commit_sha = Nothing, objectAttributes_deleted_at = Nothing, objectAttributes_in_progress_merge_commit_sha = Nothing, objectAttributes_lock_version = Nothing, objectAttributes_time_estimate = Nothing, objectAttributes_last_edited_at = Nothing, objectAttributes_last_edited_by_id = Nothing, objectAttributes_head_pipeline_id = Nothing, objectAttributes_ref_fetched = Nothing, objectAttributes_merge_jid = Nothing, objectAttributes_source = ProjectEvent {projectEvent_name = "Awesome Project", projectEvent_description = Just "Aut reprehenderit ut est.", projectEvent_web_url = "http://example.com/awesome_space/awesome_project", projectEvent_avatar_url = Nothing, projectEvent_git_ssh_url = "git@example.com:awesome_space/awesome_project.git", projectEvent_git_http_url = "http://example.com/awesome_space/awesome_project.git", projectEvent_namespace = "Awesome Space", projectEvent_visibility_level = Public, projectEvent_path_with_namespace = "awesome_space/awesome_project", projectEvent_default_branch = "master", projectEvent_homepage = Just "http://example.com/awesome_space/awesome_project", projectEvent_url = "http://example.com/awesome_space/awesome_project.git", projectEvent_ssh_url = "git@example.com:awesome_space/awesome_project.git", projectEvent_http_url = "http://example.com/awesome_space/awesome_project.git"}, objectAttributes_target = ProjectEvent {projectEvent_name = "Awesome Project", projectEvent_description = Just "Aut reprehenderit ut est.", projectEvent_web_url = "http://example.com/awesome_space/awesome_project", projectEvent_avatar_url = Nothing, projectEvent_git_ssh_url = "git@example.com:awesome_space/awesome_project.git", projectEvent_git_http_url = "http://example.com/awesome_space/awesome_project.git", projectEvent_namespace = "Awesome Space", projectEvent_visibility_level = Public, projectEvent_path_with_namespace = "awesome_space/awesome_project", projectEvent_default_branch = "master", projectEvent_homepage = Just "http://example.com/awesome_space/awesome_project", projectEvent_url = "http://example.com/awesome_space/awesome_project.git", projectEvent_ssh_url = "git@example.com:awesome_space/awesome_project.git", projectEvent_http_url = "http://example.com/awesome_space/awesome_project.git"}, objectAttributes_last_commit = CommitEvent {commitEvent_id = "da1560886d4f094c3e6c9ef40349f7d38b5d27d7", commitEvent_message = "fixed readme", commitEvent_timestamp = "2012-01-03T23:36:29+02:00", commitEvent_url = "http://example.com/awesome_space/awesome_project/commits/da1560886d4f094c3e6c9ef40349f7d38b5d27d7", commitEvent_author = CommitAuthorEvent {commitAuthorEvent_name = "GitLab dev user", commitAuthorEvent_email = "gitlabdev@dv6700.(none)"}}, objectAttributes_work_in_progress = False, objectAttributes_total_time_spent = Nothing, objectAttributes_human_total_time_spent = Nothing, objectAttributes_human_time_estimate = Nothing, objectAttributes_action = Just "open"}, mergeRequest_labels = Just [Label {label_id = Just 206, label_title = Just "API", label_color = Just "#ffffff", label_project_id = Just 14, label_created_at = Just "2013-12-03T17:15:43Z", label_updated_at = Just "2013-12-03T17:15:43Z", label_template = Just False, label_description = Just "API related issues", label_type = Just "ProjectLabel", label_group_id = Just 41}], mergeRequest_changes = MergeRequestChanges {mergeRequestChanges_author_id = Nothing, mergeRequestChanges_created_at = Nothing, mergeRequestChanges_description = Nothing, mergeRequestChanges_id = Nothing, mergeRequestChanges_iid = Nothing, mergeRequestChanges_source_branch = Nothing, mergeRequestChanges_source_project_id = Nothing, mergeRequestChanges_target_branch = Nothing, mergeRequestChanges_target_project_id = Nothing, mergeRequestChanges_title = Nothing, mergeRequestChanges_updated_at = Just (MergeRequestChange {mergeRequestChange_previous = Just "2017-09-15 16:50:55 UTC", mergeRequestChange_current = Just "2017-09-15 16:52:00 UTC"})}, mergeRequest_repository = RepositoryEvent {repositoryEvent_name = "Gitlab Test", repositoryEvent_url = "http://example.com/gitlabhq/gitlab-test.git", repositoryEvent_description = Just "Aut reprehenderit ut est.", repositoryEvent_homepage = Just "http://example.com/gitlabhq/gitlab-test", repositoryEvent_git_http_url = Nothing, repositoryEvent_git_ssh_url = Nothing, repositoryEvent_visibility_level = Nothing}}

mergeRequestGitLab_15_5_0_Haskell :: MergeRequestEvent
mergeRequestGitLab_15_5_0_Haskell =
  MergeRequestEvent {mergeRequest_object_kind = "merge_request", mergeRequest_event_type = "merge_request", mergeRequest_user = UserEvent {userEvent_name = "Joe Bloggs", userEvent_username = "joe", userEvent_avatar_url = "https://gitlab.example.com/uploads/-/system/user/avatar/5/avatar.png"}, mergeRequest_project = ProjectEvent {projectEvent_name = "test-ci-project", projectEvent_description = Just "", projectEvent_web_url = "https://gitlab.example.com/joe/test-ci-project", projectEvent_avatar_url = Nothing, projectEvent_git_ssh_url = "git@gitlab.example.com:joe/test-ci-project.git", projectEvent_git_http_url = "https://gitlab.example.com/joe/test-ci-project.git", projectEvent_namespace = "Joe Bloggs", projectEvent_visibility_level = Private, projectEvent_path_with_namespace = "joe/test-ci-project", projectEvent_default_branch = "master", projectEvent_homepage = Just "https://gitlab.example.com/joe/test-ci-project", projectEvent_url = "git@gitlab.example.com:joe/test-ci-project.git", projectEvent_ssh_url = "git@gitlab.example.com:joe/test-ci-project.git", projectEvent_http_url = "https://gitlab.example.com/joe/test-ci-project.git"}, mergeRequest_object_attributes = ObjectAttributes {objectAttributes_id = 3448, objectAttributes_target_branch = "master", objectAttributes_source_branch = "another-branch", objectAttributes_source_project_id = 7926, objectAttributes_author_id = Just 5, objectAttributes_assignee_id = Nothing, objectAttributes_assignee_ids = Just [], objectAttributes_title = "A change", objectAttributes_created_at = "2022-10-24 22:11:46 UTC", objectAttributes_updated_at = "2022-10-24 22:11:46 UTC", objectAttributes_milestone_id = Nothing, objectAttributes_state = "opened", objectAttributes_state_id = Just 1, objectAttributes_merge_status = "preparing", objectAttributes_target_project_id = 7926, objectAttributes_iid = 1, objectAttributes_description = "To test the merge request event.", objectAttributes_updated_by_id = Nothing, objectAttributes_merge_error = Nothing, objectAttributes_merge_params = Just (MergeParams {mergeParams_force_remove_source_branch = Just "1"}), objectAttributes_merge_when_pipeline_succeeds = Just False, objectAttributes_merge_user_id = Nothing, objectAttributes_merge_commit_sha = Nothing, objectAttributes_deleted_at = Nothing, objectAttributes_in_progress_merge_commit_sha = Nothing, objectAttributes_lock_version = Nothing, objectAttributes_time_estimate = Just 0, objectAttributes_last_edited_at = Nothing, objectAttributes_last_edited_by_id = Nothing, objectAttributes_head_pipeline_id = Nothing, objectAttributes_ref_fetched = Nothing, objectAttributes_merge_jid = Nothing, objectAttributes_source = ProjectEvent {projectEvent_name = "test-ci-project", projectEvent_description = Just "", projectEvent_web_url = "https://gitlab.example.com/joe/test-ci-project", projectEvent_avatar_url = Nothing, projectEvent_git_ssh_url = "git@gitlab.example.com:joe/test-ci-project.git", projectEvent_git_http_url = "https://gitlab.example.com/joe/test-ci-project.git", projectEvent_namespace = "Joe Bloggs", projectEvent_visibility_level = Private, projectEvent_path_with_namespace = "joe/test-ci-project", projectEvent_default_branch = "master", projectEvent_homepage = Just "https://gitlab.example.com/joe/test-ci-project", projectEvent_url = "git@gitlab.example.com:joe/test-ci-project.git", projectEvent_ssh_url = "git@gitlab.example.com:joe/test-ci-project.git", projectEvent_http_url = "https://gitlab.example.com/joe/test-ci-project.git"}, objectAttributes_target = ProjectEvent {projectEvent_name = "test-ci-project", projectEvent_description = Just "", projectEvent_web_url = "https://gitlab.example.com/joe/test-ci-project", projectEvent_avatar_url = Nothing, projectEvent_git_ssh_url = "git@gitlab.example.com:joe/test-ci-project.git", projectEvent_git_http_url = "https://gitlab.example.com/joe/test-ci-project.git", projectEvent_namespace = "Joe Bloggs", projectEvent_visibility_level = Private, projectEvent_path_with_namespace = "joe/test-ci-project", projectEvent_default_branch = "master", projectEvent_homepage = Just "https://gitlab.example.com/joe/test-ci-project", projectEvent_url = "git@gitlab.example.com:joe/test-ci-project.git", projectEvent_ssh_url = "git@gitlab.example.com:joe/test-ci-project.git", projectEvent_http_url = "https://gitlab.example.com/joe/test-ci-project.git"}, objectAttributes_last_commit = CommitEvent {commitEvent_id = "83c2d70f8ecb1376d1d4f1e52b7a18c560a7b95c", commitEvent_message = "A change\n", commitEvent_timestamp = "2022-10-24T23:10:03+01:00", commitEvent_url = "https://gitlab.example.com/joe/test-ci-project/-/commit/83c2d70f8ecb1376d1d4f1e52b7a18c560a7b95c", commitEvent_author = CommitAuthorEvent {commitAuthorEvent_name = "Joe Bloggs", commitAuthorEvent_email = "robstewart57@gmail.com"}}, objectAttributes_work_in_progress = False, objectAttributes_total_time_spent = Just 0, objectAttributes_human_total_time_spent = Nothing, objectAttributes_human_time_estimate = Nothing, objectAttributes_action = Just "open"}, mergeRequest_labels = Just [], mergeRequest_changes = MergeRequestChanges {mergeRequestChanges_author_id = Nothing, mergeRequestChanges_created_at = Nothing, mergeRequestChanges_description = Nothing, mergeRequestChanges_id = Nothing, mergeRequestChanges_iid = Nothing, mergeRequestChanges_source_branch = Nothing, mergeRequestChanges_source_project_id = Nothing, mergeRequestChanges_target_branch = Nothing, mergeRequestChanges_target_project_id = Nothing, mergeRequestChanges_title = Nothing, mergeRequestChanges_updated_at = Nothing}, mergeRequest_repository = RepositoryEvent {repositoryEvent_name = "test-ci-project", repositoryEvent_url = "git@gitlab.example.com:joe/test-ci-project.git", repositoryEvent_description = Just "", repositoryEvent_homepage = Just "https://gitlab.example.com/joe/test-ci-project", repositoryEvent_git_http_url = Nothing, repositoryEvent_git_ssh_url = Nothing, repositoryEvent_visibility_level = Nothing}}

-- | tests that this fix works:
-- https://gitlab.com/robstewart57/gitlab-haskell/-/commit/d1ca1037944616ac940284c5f8e49b5d9bcbf83c
mergeRequestGitLab_15_5_0_maybe_descriptions_Haskell :: MergeRequestEvent
mergeRequestGitLab_15_5_0_maybe_descriptions_Haskell =
  MergeRequestEvent {mergeRequest_object_kind = "merge_request", mergeRequest_event_type = "merge_request", mergeRequest_user = UserEvent {userEvent_name = "Bloggs, Joe", userEvent_username = "joe123", userEvent_avatar_url = "https://secure.gravatar.com/avatar/df1b70b1fd7d7eaaa5f9a41f5f5093c0?s=80&d=identicon"}, mergeRequest_project = ProjectEvent {projectEvent_name = "proj1", projectEvent_description = Nothing, projectEvent_web_url = "https://gitlab.example.com/grp1/grp2/proj1", projectEvent_avatar_url = Nothing, projectEvent_git_ssh_url = "git@gitlab.example.com:grp1/grp2/proj1.git", projectEvent_git_http_url = "https://gitlab.example.com/grp1/grp2/proj1.git", projectEvent_namespace = "grp2", projectEvent_visibility_level = Private, projectEvent_path_with_namespace = "grp1/grp2/proj1", projectEvent_default_branch = "main", projectEvent_homepage = Just "https://gitlab.example.com/grp1/grp2/proj1", projectEvent_url = "git@gitlab.example.com:grp1/grp2/proj1.git", projectEvent_ssh_url = "git@gitlab.example.com:grp1/grp2/proj1.git", projectEvent_http_url = "https://gitlab.example.com/grp1/grp2/proj1.git"}, mergeRequest_object_attributes = ObjectAttributes {objectAttributes_id = 3461, objectAttributes_target_branch = "main", objectAttributes_source_branch = "main", objectAttributes_source_project_id = 49668, objectAttributes_author_id = Just 2312, objectAttributes_assignee_id = Nothing, objectAttributes_assignee_ids = Just [], objectAttributes_title = "the MR title", objectAttributes_created_at = "2022-10-25 18:51:48 UTC", objectAttributes_updated_at = "2022-10-25 18:51:48 UTC", objectAttributes_milestone_id = Nothing, objectAttributes_state = "opened", objectAttributes_state_id = Just 1, objectAttributes_merge_status = "preparing", objectAttributes_target_project_id = 43593, objectAttributes_iid = 14, objectAttributes_description = "", objectAttributes_updated_by_id = Nothing, objectAttributes_merge_error = Nothing, objectAttributes_merge_params = Just (MergeParams {mergeParams_force_remove_source_branch = Nothing}), objectAttributes_merge_when_pipeline_succeeds = Just False, objectAttributes_merge_user_id = Nothing, objectAttributes_merge_commit_sha = Nothing, objectAttributes_deleted_at = Nothing, objectAttributes_in_progress_merge_commit_sha = Nothing, objectAttributes_lock_version = Nothing, objectAttributes_time_estimate = Just 0, objectAttributes_last_edited_at = Nothing, objectAttributes_last_edited_by_id = Nothing, objectAttributes_head_pipeline_id = Nothing, objectAttributes_ref_fetched = Nothing, objectAttributes_merge_jid = Nothing, objectAttributes_source = ProjectEvent {projectEvent_name = "proj1", projectEvent_description = Nothing, projectEvent_web_url = "https://gitlab.example.com/joe123/proj1", projectEvent_avatar_url = Nothing, projectEvent_git_ssh_url = "git@gitlab.example.com:joe123/proj1.git", projectEvent_git_http_url = "https://gitlab.example.com/joe123/proj1.git", projectEvent_namespace = "Bloggs, Joe", projectEvent_visibility_level = Private, projectEvent_path_with_namespace = "joe123/proj1", projectEvent_default_branch = "main", projectEvent_homepage = Just "https://gitlab.example.com/joe123/proj1", projectEvent_url = "git@gitlab.example.com:joe123/proj1.git", projectEvent_ssh_url = "git@gitlab.example.com:joe123/proj1.git", projectEvent_http_url = "https://gitlab.example.com/joe123/proj1.git"}, objectAttributes_target = ProjectEvent {projectEvent_name = "proj1", projectEvent_description = Nothing, projectEvent_web_url = "https://gitlab.example.com/grp1/grp2/proj1", projectEvent_avatar_url = Nothing, projectEvent_git_ssh_url = "git@gitlab.example.com:grp1/grp2/proj1.git", projectEvent_git_http_url = "https://gitlab.example.com/grp1/grp2/proj1.git", projectEvent_namespace = "grp2", projectEvent_visibility_level = Private, projectEvent_path_with_namespace = "grp1/grp2/proj1", projectEvent_default_branch = "main", projectEvent_homepage = Just "https://gitlab.example.com/grp1/grp2/proj1", projectEvent_url = "git@gitlab.example.com:grp1/grp2/proj1.git", projectEvent_ssh_url = "git@gitlab.example.com:grp1/grp2/proj1.git", projectEvent_http_url = "https://gitlab.example.com/grp1/grp2/proj1.git"}, objectAttributes_last_commit = CommitEvent {commitEvent_id = "249d94fdc92825a2ca22c4fa74c511ec5f03e086", commitEvent_message = "Updated README\n", commitEvent_timestamp = "2022-10-08T10:37:28+01:00", commitEvent_url = "https://gitlab.example.com/grp1/grp2/proj1/-/commit/249d94fdc92825a2ca22c4fa74c511ec5f03e086", commitEvent_author = CommitAuthorEvent {commitAuthorEvent_name = "Hill, Harry", commitAuthorEvent_email = "harry@gmail.com"}}, objectAttributes_work_in_progress = True, objectAttributes_total_time_spent = Just 0, objectAttributes_human_total_time_spent = Nothing, objectAttributes_human_time_estimate = Nothing, objectAttributes_action = Just "open"}, mergeRequest_labels = Just [], mergeRequest_changes = MergeRequestChanges {mergeRequestChanges_author_id = Nothing, mergeRequestChanges_created_at = Nothing, mergeRequestChanges_description = Nothing, mergeRequestChanges_id = Nothing, mergeRequestChanges_iid = Nothing, mergeRequestChanges_source_branch = Nothing, mergeRequestChanges_source_project_id = Nothing, mergeRequestChanges_target_branch = Nothing, mergeRequestChanges_target_project_id = Nothing, mergeRequestChanges_title = Nothing, mergeRequestChanges_updated_at = Nothing}, mergeRequest_repository = RepositoryEvent {repositoryEvent_name = "proj1", repositoryEvent_url = "git@gitlab.example.com:grp1/grp2/proj1.git", repositoryEvent_description = Nothing, repositoryEvent_homepage = Just "https://gitlab.example.com/grp1/grp2/proj1", repositoryEvent_git_http_url = Nothing, repositoryEvent_git_ssh_url = Nothing, repositoryEvent_visibility_level = Nothing}}

buildHaskell :: Build
buildHaskell =
  Build {build_object_kind = "build", build_ref = "master", build_tag = False, build_before_sha = "19d1543c2f5ea888e3c69019bd7e3daff228fcbc", build_sha = "12fc8bb3a8b3f52c5bffa020e6d362027c724359", build_retries_count = 0, build_build_id = 47423, build_build_name = "junit", build_build_stage = "test", build_build_status = "created", build_created_at = "2024-11-01 20:15:34 UTC", build_started_at = Nothing, build_finished_at = Nothing, build_duration = Nothing, build_queued_duration = Nothing, build_allow_failure = False, build_failure_reason = "unknown_failure", build_pipeline_id = 32611, build_runner = Nothing, build_project_id = 25039, build_project_name = "Joe / repo-name", build_user = User {user_id = 2682, user_username = "joe", user_bio = Nothing, user_two_factor_enabled = Nothing, user_last_sign_in_at = Nothing, user_current_sign_in_at = Nothing, user_last_activity_on = Nothing, user_skype = Nothing, user_twitter = Nothing, user_website_url = Nothing, user_theme_id = Nothing, user_color_scheme_id = Nothing, user_external = Nothing, user_private_profile = Nothing, user_projects_limit = Nothing, user_can_create_group = Nothing, user_can_create_project = Nothing, user_public_email = Nothing, user_organization = Nothing, user_job_title = Nothing, user_pronouns = Nothing, user_linkedin = Nothing, user_confirmed_at = Nothing, user_identities = Nothing, user_name = "Joe", user_email = Just "[REDACTED]", user_followers = Nothing, user_bot = Nothing, user_following = Nothing, user_state = Nothing, user_avatar_url = Just "https://secure.gravatar.com/avatar/abcde", user_web_url = Nothing, user_location = Nothing, user_extern_uid = Nothing, user_group_id_for_saml = Nothing, user_discussion_locked = Nothing, user_created_at = Nothing, user_note = Nothing, user_password = Nothing, user_force_random_password = Nothing, user_providor = Nothing, user_reset_password = Nothing, user_skip_confirmation = Nothing, user_view_diffs_file_by_file = Nothing}, build_commit = BuildCommit {build_commit_id = 32611, build_commit_name = Nothing, build_commit_sha = "12fc8bb3a8b3f52c5bffa020e6d362027c724359", build_commit_message = "the message", build_commit_author_name = "Joe", build_commit_author_email = "joe@gmail.com", build_commit_author_url = "mailto:joe@gmail.com", build_commit_status = "created", build_commit_duration = Nothing, build_commit_started_at = Nothing, build_commit_finished_at = Nothing}, build_repository = Repository {repository_id = Nothing, repository_name = "repo-name", repository_type = Nothing, repository_path = Nothing, repository_mode = Nothing}, build_project = BuildProject {build_project_project_id = 25039, build_project_project_name = "repo-name", build_project_description = Nothing, build_project_web_url = "https://example.com/joe/repo-name", build_project_avatar_url = Nothing, build_project_git_ssh_url = "git@example.com:joe/repo-name.git", build_project_git_http_url = "https://example.com/joe/repo-name.git", build_project_namespace = "Joe", build_project_visibility_level = 0, build_project_path_with_namespace = "joe/repo-name", build_project_default_branch = "master", build_project_ci_config_path = Nothing}, build_environment = Nothing}

build2Haskell :: Build
build2Haskell =
  Build {build_object_kind = "build", build_ref = "master", build_tag = False, build_before_sha = "edcba", build_sha = "abcde", build_retries_count = 0, build_build_id = 47500, build_build_name = "junit", build_build_stage = "test", build_build_status = "failed", build_created_at = "2024-11-02 18:29:00 UTC", build_started_at = Just "2024-11-02 18:29:01 UTC", build_finished_at = Just "2024-11-02 18:29:06 UTC", build_duration = Just 4.543246, build_queued_duration = Just 1.08065, build_allow_failure = False, build_failure_reason = "script_failure", build_pipeline_id = 32685, build_runner = Just (Runner {runner_id = 17, runner_description = "", runner_runner_type = "instance_type", runner_active = True, runner_is_shared = True, runner_tags = []}), build_project_id = 25078, build_project_name = "joe/proj-name", build_user = User {user_id = 742, user_username = "jg2078", user_bio = Nothing, user_two_factor_enabled = Nothing, user_last_sign_in_at = Nothing, user_current_sign_in_at = Nothing, user_last_activity_on = Nothing, user_skype = Nothing, user_twitter = Nothing, user_website_url = Nothing, user_theme_id = Nothing, user_color_scheme_id = Nothing, user_external = Nothing, user_private_profile = Nothing, user_projects_limit = Nothing, user_can_create_group = Nothing, user_can_create_project = Nothing, user_public_email = Nothing, user_organization = Nothing, user_job_title = Nothing, user_pronouns = Nothing, user_linkedin = Nothing, user_confirmed_at = Nothing, user_identities = Nothing, user_name = "Goddard, Jamie", user_email = Just "[REDACTED]", user_followers = Nothing, user_bot = Nothing, user_following = Nothing, user_state = Nothing, user_avatar_url = Just "https://secure.gravatar.com/avatar/abc", user_web_url = Nothing, user_location = Nothing, user_extern_uid = Nothing, user_group_id_for_saml = Nothing, user_discussion_locked = Nothing, user_created_at = Nothing, user_note = Nothing, user_password = Nothing, user_force_random_password = Nothing, user_providor = Nothing, user_reset_password = Nothing, user_skip_confirmation = Nothing, user_view_diffs_file_by_file = Nothing}, build_commit = BuildCommit {build_commit_id = 32685, build_commit_name = Nothing, build_commit_sha = "abcde", build_commit_message = "message name", build_commit_author_name = "joe", build_commit_author_email = "joe@gmail.com", build_commit_author_url = "mailto:joe@gmail.com", build_commit_status = "failed", build_commit_duration = Just 4, build_commit_started_at = Just "2024-11-02 18:29:01 UTC", build_commit_finished_at = Just "2024-11-02 18:29:06 UTC"}, build_repository = Repository {repository_id = Nothing, repository_name = "proj-name", repository_type = Nothing, repository_path = Nothing, repository_mode = Nothing}, build_project = BuildProject {build_project_project_id = 25078, build_project_project_name = "proj-name", build_project_description = Nothing, build_project_web_url = "https://example.com/joe/proj-name", build_project_avatar_url = Nothing, build_project_git_ssh_url = "git@example.com:joe/proj-name.git", build_project_git_http_url = "https://example.com/joe/proj-name.git", build_project_namespace = "Joe", build_project_visibility_level = 0, build_project_path_with_namespace = "joe/proj-name", build_project_default_branch = "master", build_project_ci_config_path = Nothing}, build_environment = Nothing}

pipelineHaskell :: PipelineEvent
pipelineHaskell =
  PipelineEvent {pipeline_event_object_kind = "pipeline", pipeline_event_object_attributes = PipelineObjectAttributes {pipeline_object_attributes_id = 32617, pipeline_object_attributes_iid = 4, pipeline_object_attributes_name = Nothing, pipeline_object_attributes_ref = "master", pipeline_object_attributes_tag = False, pipeline_object_attributes_sha = "abcde", pipeline_object_attributes_before_sha = "edcba", pipeline_object_attributes_source = "push", pipeline_object_attributes_status = "failed", pipeline_object_attributes_detailed_status = "failed", pipeline_object_attributes_stages = ["test"], pipeline_object_attributes_created_at = "2024-11-01 21:58:31 UTC", pipeline_object_attributes_finished_at = "2024-11-01 21:58:35 UTC", pipeline_object_attributes_duration = 3, pipeline_object_attributes_queued_duration = Nothing, pipeline_object_attributes_variables = [], pipeline_object_attributes_url = "https://example.com/joe/proj-name/-/pipelines/32617"}, pipeline_event_merge_request = Nothing, pipeline_event_user = User {user_id = 899, user_username = "joe", user_bio = Nothing, user_two_factor_enabled = Nothing, user_last_sign_in_at = Nothing, user_current_sign_in_at = Nothing, user_last_activity_on = Nothing, user_skype = Nothing, user_twitter = Nothing, user_website_url = Nothing, user_theme_id = Nothing, user_color_scheme_id = Nothing, user_external = Nothing, user_private_profile = Nothing, user_projects_limit = Nothing, user_can_create_group = Nothing, user_can_create_project = Nothing, user_public_email = Nothing, user_organization = Nothing, user_job_title = Nothing, user_pronouns = Nothing, user_linkedin = Nothing, user_confirmed_at = Nothing, user_identities = Nothing, user_name = "Joe", user_email = Just "[REDACTED]", user_followers = Nothing, user_bot = Nothing, user_following = Nothing, user_state = Nothing, user_avatar_url = Just "https://secure.gravatar.com/avatar/abc", user_web_url = Nothing, user_location = Nothing, user_extern_uid = Nothing, user_group_id_for_saml = Nothing, user_discussion_locked = Nothing, user_created_at = Nothing, user_note = Nothing, user_password = Nothing, user_force_random_password = Nothing, user_providor = Nothing, user_reset_password = Nothing, user_skip_confirmation = Nothing, user_view_diffs_file_by_file = Nothing}, pipeline_event_project = BuildProject {build_project_project_id = 24472, build_project_project_name = "proj-name", build_project_description = Nothing, build_project_web_url = "https://example.com/joe/proj-name", build_project_avatar_url = Nothing, build_project_git_ssh_url = "git@example.com:joe/proj-name.git", build_project_git_http_url = "https://example.com/joe/proj-name.git", build_project_namespace = "Joe", build_project_visibility_level = 0, build_project_path_with_namespace = "joe/proj-name", build_project_default_branch = "master", build_project_ci_config_path = Nothing}, pipeline_event_commit = CommitEvent {commitEvent_id = "abcde", commitEvent_message = "the commit message", commitEvent_timestamp = "2024-11-01T21:58:31+00:00", commitEvent_url = "https://example.com/joe/proj-name/-/commit/abcde", commitEvent_author = CommitAuthorEvent {commitAuthorEvent_name = "Joe", commitAuthorEvent_email = "12345+Joe@users.noreply.github.com"}}, pipeline_event_builds = [PipelineBuild {pipeline_build_id = 47429, pipeline_build_stage = "test", pipeline_build_name = "junit", pipeline_build_status = "failed", pipeline_build_created_at = "2024-11-01 21:58:31 UTC", pipeline_build_started_at = "2024-11-01 21:58:31 UTC", pipeline_build_finished_at = "2024-11-01 21:58:35 UTC", pipeline_build_duration = 3.889573, pipeline_build_queued_duration = 0.19131, pipeline_build_failure_reason = "script_failure", pipeline_build_when = "on_success", pipeline_build_manual = False, pipeline_build_allow_failure = False, pipeline_build_user = UserEvent {userEvent_name = "Joe", userEvent_username = "joe", userEvent_avatar_url = "https://secure.gravatar.com/avatar/abc"}, pipeline_build_runner = Runner {runner_id = 15, runner_description = "", runner_runner_type = "instance_type", runner_active = True, runner_is_shared = True, runner_tags = []}, pipeline_build_artifacts_file = ArtifactsFile {artifacts_file_filename = Nothing, artifacts_file_size = Nothing}, pipeline_build_environment = Nothing}]}

pipeline2Haskell :: PipelineEvent
pipeline2Haskell =
  PipelineEvent {pipeline_event_object_kind = "pipeline", pipeline_event_object_attributes = PipelineObjectAttributes {pipeline_object_attributes_id = 32686, pipeline_object_attributes_iid = 2, pipeline_object_attributes_name = Nothing, pipeline_object_attributes_ref = "master", pipeline_object_attributes_tag = False, pipeline_object_attributes_sha = "abcde", pipeline_object_attributes_before_sha = "edcba", pipeline_object_attributes_source = "push", pipeline_object_attributes_status = "failed", pipeline_object_attributes_detailed_status = "failed", pipeline_object_attributes_stages = ["test"], pipeline_object_attributes_created_at = "2024-11-02 18:32:17 UTC", pipeline_object_attributes_finished_at = "2024-11-02 18:32:23 UTC", pipeline_object_attributes_duration = 4.0, pipeline_object_attributes_queued_duration = Just 1.0, pipeline_object_attributes_variables = [], pipeline_object_attributes_url = "https://example.com/joe/proj-name/-/pipelines/32686"}, pipeline_event_merge_request = Nothing, pipeline_event_user = User {user_id = 742, user_username = "joe", user_bio = Nothing, user_two_factor_enabled = Nothing, user_last_sign_in_at = Nothing, user_current_sign_in_at = Nothing, user_last_activity_on = Nothing, user_skype = Nothing, user_twitter = Nothing, user_website_url = Nothing, user_theme_id = Nothing, user_color_scheme_id = Nothing, user_external = Nothing, user_private_profile = Nothing, user_projects_limit = Nothing, user_can_create_group = Nothing, user_can_create_project = Nothing, user_public_email = Nothing, user_organization = Nothing, user_job_title = Nothing, user_pronouns = Nothing, user_linkedin = Nothing, user_confirmed_at = Nothing, user_identities = Nothing, user_name = "Joe", user_email = Just "[REDACTED]", user_followers = Nothing, user_bot = Nothing, user_following = Nothing, user_state = Nothing, user_avatar_url = Just "https://secure.gravatar.com/avatar/abc", user_web_url = Nothing, user_location = Nothing, user_extern_uid = Nothing, user_group_id_for_saml = Nothing, user_discussion_locked = Nothing, user_created_at = Nothing, user_note = Nothing, user_password = Nothing, user_force_random_password = Nothing, user_providor = Nothing, user_reset_password = Nothing, user_skip_confirmation = Nothing, user_view_diffs_file_by_file = Nothing}, pipeline_event_project = BuildProject {build_project_project_id = 25078, build_project_project_name = "proj-name", build_project_description = Nothing, build_project_web_url = "https://example.com/joe/proj-name", build_project_avatar_url = Nothing, build_project_git_ssh_url = "git@example.com:joe/proj-name.git", build_project_git_http_url = "https://example.com/joe/proj-name.git", build_project_namespace = "Joe", build_project_visibility_level = 0, build_project_path_with_namespace = "joe/proj-name", build_project_default_branch = "master", build_project_ci_config_path = Nothing}, pipeline_event_commit = CommitEvent {commitEvent_id = "abcde", commitEvent_message = "the commit message", commitEvent_timestamp = "2024-11-02T18:32:16+00:00", commitEvent_url = "https://example.com/joe/proj-name/-/commit/abcde", commitEvent_author = CommitAuthorEvent {commitAuthorEvent_name = "joe", commitAuthorEvent_email = "joe@gmail.com"}}, pipeline_event_builds = [PipelineBuild {pipeline_build_id = 47501, pipeline_build_stage = "test", pipeline_build_name = "junit", pipeline_build_status = "failed", pipeline_build_created_at = "2024-11-02 18:32:18 UTC", pipeline_build_started_at = "2024-11-02 18:32:18 UTC", pipeline_build_finished_at = "2024-11-02 18:32:23 UTC", pipeline_build_duration = 4.428372, pipeline_build_queued_duration = 0.561693, pipeline_build_failure_reason = "script_failure", pipeline_build_when = "on_success", pipeline_build_manual = False, pipeline_build_allow_failure = False, pipeline_build_user = UserEvent {userEvent_name = "Joe", userEvent_username = "joe", userEvent_avatar_url = "https://secure.gravatar.com/avatar/abc"}, pipeline_build_runner = Runner {runner_id = 15, runner_description = "", runner_runner_type = "instance_type", runner_active = True, runner_is_shared = True, runner_tags = []}, pipeline_build_artifacts_file = ArtifactsFile {artifacts_file_filename = Nothing, artifacts_file_size = Nothing}, pipeline_build_environment = Nothing}]}
