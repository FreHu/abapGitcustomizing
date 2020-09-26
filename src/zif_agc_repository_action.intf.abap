INTERFACE zif_agc_repository_action
  PUBLIC .

  METHODS push.

  METHODS pull
    RETURNING VALUE(rt_imported_objects) TYPE zif_agc_ui=>ty_t_customizing_ui.

ENDINTERFACE.
