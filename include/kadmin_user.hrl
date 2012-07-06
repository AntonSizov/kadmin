-ifndef(kadmin_user_hrl).
-define(kadmin_user_hrl, included).

%% user's states
-define(KU_ACTIVE, 'Active').
-define(KU_BLOCKED, 'Blocked').
-define(KU_DEACTIVATED, 'Deactivated').

%% user's access levels
-define(AL_FULL, [system_administrator]). % lists:member(Elem, List)


-endif. % kadmin_user_hrl


% -define(SYSTEM_ADMINISTRATOR, 1).
% -define(CUSTOMER_ACCOUNT_ADMINISTRATOR, 1).
% -define(CAN_ADD_PACKAGES, 1).
% -define(ALLOW_MSISDN_CHANGE, 1).

% -define(CUSTOMER_CARE_BASIC, 1).

% -define(CUSTOMER_CARE_ADVANCED, 1).
% -define(SUPERVISOR, 1). % CAN SEE MESSAGE TEXT

% -define(STATISTICAL_INTERFACE, 1).
% -define(OA_AND_M, 1).
% -define(OA_AND_M_DEBUG, 1).
% -define(ENGINE_ADMINISTRATION, 1).
% -define(GENEGAL_ENGINE_CONFIGURATION, 1).
% -define(SMS_CONFIGURATION, 1).
% -define(MMS_CONFIGURATION, 1).

% -define(SYSTEM_STATISTIC, 1).
% -define(CUSTOMER_ORIGINATOR_APPROVAL, 1).
% -define(SMPP_DOWNLINKS, 1).
% -define(CRM_MOBILE_ACCOUNTS, 1).
% -define(CRM_FIXED_LINE_ACCOUNTS, 1).
% -define(CRM_CREDIT_CARD_ACCOUNTS, 1).
% -define(ENABLE_ACCESS_FROM_ALL_BROWSERS, 1).
